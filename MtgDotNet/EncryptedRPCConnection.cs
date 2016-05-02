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
        protected byte[] secretKey;
        protected byte[] publicKey;
        protected byte[] ephemeralSecret;
        protected byte[] sessionPublic;
        protected byte[][] authorizedKeys;
        protected const int nonceBytes = 24;

        public EncryptedRPCConnection(MtgDotNet.Sys.IFramer framer, MtgDotNet.Sys.ITransport transport, byte[] secret, byte[][] authorizedKeys) : base(framer, transport)
        {
            this.authorizedKeys = authorizedKeys;
            this.secretKey = secret;
            this.publicKey = Sodium.PublicKeyAuth.ExtractEd25519PublicKeyFromEd25519SecretKey(secret);
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
            byte[] remotePublicKey = new byte[this.publicKey.Length];
            Sodium.KeyPair ephemeralPair = Sodium.PublicKeyBox.GenerateKeyPair();
            byte[] signedSessionKey;
            byte[] remoteSignedSessionKey;
            byte[] remoteSessionKey;
            byte[] frame;
            Task sendTask;

            // Store the ephemeral private key.
            this.ephemeralSecret = ephemeralPair.PrivateKey;

            signedSessionKey = Sodium.PublicKeyAuth.Sign(ephemeralPair.PublicKey, this.secretKey);
            remoteSignedSessionKey = new byte[signedSessionKey.Length];

            // Send the signing key and the signed session key.
            sendTask = this.SendFrameMulti(this.publicKey, signedSessionKey);

            frame = await this.ReceiveFrame().ConfigureAwait(false);
            if (frame.Length != (signedSessionKey.Length + this.publicKey.Length))
            {
                throw new Exception("Handshake error: handshake data is the wrong length");
            }

            Array.Copy(frame, remotePublicKey, remotePublicKey.Length);
            Array.Copy(frame, remotePublicKey.Length, remoteSignedSessionKey, 0, remoteSignedSessionKey.Length);

            try
            {
                this.authorizedKeys.First<byte[]>(key => key == null || this.KeyEqual(key, remotePublicKey));
            }
            catch (InvalidOperationException) {
                throw new ClientNotAuthorizedException(remotePublicKey);
            }

            // We recognize the signing key, so retrieve the signed session key.
            remoteSessionKey = Sodium.PublicKeyAuth.Verify(remoteSignedSessionKey, remotePublicKey);
            this.sessionPublic = remoteSessionKey;

            await sendTask.ConfigureAwait(false);
        }

        public byte[] EncryptData(byte[] data)
        {
            byte[] nonce = Sodium.PublicKeyBox.GenerateNonce();
            byte[] encrypted = Sodium.PublicKeyBox.Create(data, nonce, this.ephemeralSecret, this.sessionPublic);

            // Ugghhh, just expose the size constants already libsodium-net. Jeez. I'm not made of array copies here.
            byte[] message = new byte[encrypted.Length + nonce.Length];
            nonce.CopyTo(message, 0);

            Array.Copy(encrypted, 0, message, nonce.Length, encrypted.Length);

            return message;
        }

        public byte[] DecryptData(byte[] message)
        {
            byte[] nonce = new byte[EncryptedRPCConnection.nonceBytes];
            byte[] cipherText = Enumerable.Repeat<byte>(0, message.Length - EncryptedRPCConnection.nonceBytes).ToArray();
            byte[] data;

            Array.Copy(message, nonce, nonce.Length);
            Array.Copy(message, nonce.Length, cipherText, 0, cipherText.Length);

            data = Sodium.PublicKeyBox.Open(cipherText, nonce, this.ephemeralSecret, this.sessionPublic);

            return data;
        }

        public async override Task Connect()
        {
            await base.Connect().ConfigureAwait(false);
            await this.PerformHandshake().ConfigureAwait(false);
        }

        public async override Task SendResponse(RPCResponse response)
        {
            string message = JsonConvert.SerializeObject(response);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            byte[] cipherText = this.EncryptData(data);
            await this.SendFrameMulti(cipherText).ConfigureAwait(false);
        }

        public async override Task<RPCResponse> ReceiveResponse()
        {
            byte[] frame;
            byte[] data;
            string message;
            
            frame = await this.ReceiveFrame().ConfigureAwait(false);
            data = this.DecryptData(frame);
            message = System.Text.UTF8Encoding.UTF8.GetString(data);

            return JsonConvert.DeserializeObject<RPCResponse>(message);
        }

        public async override Task SendRequest(RPCRequest request)
        {
            string message = JsonConvert.SerializeObject(request);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            byte[] cipherText = this.EncryptData(data);
            await this.SendFrame(cipherText).ConfigureAwait(false);
        }

        public async override Task<RPCRequest> ReceiveRequest()
        {
            byte[] frame;
            byte[] data;
            string message;

            frame = await this.ReceiveFrame().ConfigureAwait(false);
            data = this.DecryptData(frame);
            message = System.Text.UTF8Encoding.UTF8.GetString(data);
            return JsonConvert.DeserializeObject<RPCRequest>(message);
        }
    }
}
