using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using MtgDotNet.ProtocolObjects;

namespace MtgDotNet
{
    public class EncryptedRPCConnection : RPCConnection, IRPCConnection
    {
        protected byte[] localNonce;
        protected byte[] remoteNonce;
        protected byte[] secretKey;
        protected byte[] publicKey;
        protected byte[] sessionKey;
        protected byte[][] authorizedKeys;
        protected const int paddingBytes = 16;

        public EncryptedRPCConnection(MtgDotNet.Sys.IFramer framer, MtgDotNet.Sys.ITransport transport, byte[] secret, byte[][] authorizedKeys) : base(framer, transport)
        {
            this.authorizedKeys = authorizedKeys;
            this.secretKey = secret;
            this.publicKey = Sodium.ScalarMult.Base(secret);
        }

        protected byte[] SharedNonce(byte[] local, byte[] remote)
        {
            byte[] shared = new byte[this.localNonce.Length];
            for (int i = 0; i < shared.Length; i++)
            {
                shared[i] = (byte)(local[i] ^ remote[i]);
            }
            return shared;
        }

        protected byte[] GenerateSessionKey(byte[] remotePublicKey)
        {
            byte[] dhSecret = Sodium.ScalarMult.Mult(this.secretKey, remotePublicKey);
            byte[] sharedNonce = this.SharedNonce(this.localNonce, this.remoteNonce);
            return Sodium.GenericHash.Hash(sharedNonce, dhSecret, dhSecret.Length);
        }

        protected bool KeyEqual(byte[] key1, byte[]key2)
        {
            System.Diagnostics.Debug.Assert(key1.Length == key2.Length);
            System.Diagnostics.Debug.Assert(key1.Length == Sodium.PublicKeyBox.PublicKeyBytes);
            for (int i = 0; i < Sodium.PublicKeyBox.PublicKeyBytes; i++)
            {
                if (key1[i] != key2[i])
                {
                    return false;
                }
            }
            return true;
        }

        public async Task PerformHandshake()
        {
            byte[] remoteNonce = new byte[this.localNonce.Length];
            byte[] remotePublicKey = new byte[this.publicKey.Length];
            byte[] frame;
            Task sendTask;

            // Send the nonce and public key
            sendTask = this.SendFrameMulti(this.localNonce, this.publicKey);

            frame = await this.ReceiveFrame();
            if (frame.Length != (this.publicKey.Length + this.localNonce.Length))
            {
                throw new Exception("Handshake error: handshake data is the wrong length");
            }

            Array.Copy(frame, remoteNonce, remoteNonce.Length);
            Array.Copy(frame, remoteNonce.Length, remotePublicKey, 0, this.publicKey.Length);

            try
            {
                this.authorizedKeys.First<byte[]>(key => key == null || this.KeyEqual(key, remotePublicKey));
            }
            catch (InvalidOperationException) {
                throw new ClientNotAuthorizedException(remotePublicKey);
            }

            this.remoteNonce = remoteNonce;
            this.sessionKey = this.GenerateSessionKey(remotePublicKey);

            await sendTask;
        }

        public byte[] EncryptData(byte[] data)
        {
            byte[] sharedNonce = new byte[this.localNonce.Length];

            for (int i = 0; i < sharedNonce.Length; i++)
            {
                sharedNonce[i] = (byte)(this.localNonce[i] ^ this.remoteNonce[i]);
            }

            byte[] encrypted = Sodium.SecretBox.Create(data, sharedNonce, this.sessionKey);

            // Ugghhh, just expose the size constants already libsodium-net. Jeez. I'm not made of array copies here.
            byte[] message = new byte[encrypted.Length - EncryptedRPCConnection.paddingBytes + sharedNonce.Length];
            this.localNonce.CopyTo(message, 0);

            // This fun little dance is because the libsodium-net author didn't use the *_easy API, so messages come out
            // with paddingBytes of zero padding on the front, and need it added back on before decryption. Le sigh.
            Array.Copy(encrypted, EncryptedRPCConnection.paddingBytes, message, this.localNonce.Length, encrypted.Length - EncryptedRPCConnection.paddingBytes);

            return message;
        }

        public byte[] DecryptData(byte[] message, out byte[] remoteNonce)
        {
            byte[] sharedNonce = new byte[this.localNonce.Length];
            byte[] cipherText = new byte[message.Length - sharedNonce.Length + EncryptedRPCConnection.paddingBytes];
            byte[] data;

            for (int i = 0; i < sharedNonce.Length; i++)
            {
                sharedNonce[i] = (byte)(this.localNonce[i] ^ message[i]);
            }

            // Ciphertext needs to start with zero-padding.
            for (int i = 0; i < EncryptedRPCConnection.paddingBytes; i++)
            {
                cipherText[i] = 0;
            }

            Array.Copy(message, sharedNonce.Length, cipherText, EncryptedRPCConnection.paddingBytes, cipherText.Length - EncryptedRPCConnection.paddingBytes);

            data = Sodium.SecretBox.Open(cipherText, sharedNonce, this.sessionKey);

            // Copy the remote nonce into the nonce buffer so we can return it.
            Array.Copy(message, sharedNonce, sharedNonce.Length);
            remoteNonce = sharedNonce;

            return data;
        }

        public async override Task Connect()
        {
            this.localNonce = Sodium.SecretBox.GenerateNonce();
            await base.Connect();
            await this.PerformHandshake();
        }

        public async override Task SendResponse(RPCResponse response)
        {
            this.localNonce = Sodium.SecretBox.GenerateNonce();

            string message = JsonConvert.SerializeObject(response);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            byte[] cipherText = this.EncryptData(data);
            await this.SendFrame(cipherText);
        }

        public async override Task<RPCResponse> ReceiveResponse()
        {
            byte[] remoteNonce;
            byte[] frame;
            byte[] data;
            string message;
            
            frame = await this.ReceiveFrame();
            data = this.DecryptData(frame, out remoteNonce);
            this.remoteNonce = remoteNonce;
            message = System.Text.UTF8Encoding.UTF8.GetString(data);

            return JsonConvert.DeserializeObject<RPCResponse>(message);
        }

        public async override Task SendRequest(RPCRequest request)
        {
            this.localNonce = Sodium.SecretBox.GenerateNonce();

            string message = JsonConvert.SerializeObject(request);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            byte[] cipherText = this.EncryptData(data);
            await this.SendFrame(cipherText);
        }

        public async override Task<RPCRequest> ReceiveRequest()
        {
            byte[] frame;
            byte[] sharedNonce;
            byte[] data;
            string message;

            frame = await this.ReceiveFrame();
            data = this.DecryptData(frame, out sharedNonce);
            message = System.Text.UTF8Encoding.UTF8.GetString(data);
            return JsonConvert.DeserializeObject<RPCRequest>(message);
        }
    }
}
