using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using MtgDotNet.ProtocolObjects;

namespace MtgDotNet
{
    public class RPCConnection : IDisposable, MtgDotNet.IRPCConnection
    {
        protected MtgDotNet.Sys.IFramer framer;
        protected MtgDotNet.Sys.ITransport transport;
        protected Dictionary<int, RPCResult> resultBucket = new Dictionary<int, RPCResult>();
        protected Task nextResponseTask = null;

        public RPCConnection(MtgDotNet.Sys.IFramer framer, MtgDotNet.Sys.ITransport transport)
        {
            this.framer = framer;
            this.transport = transport;
        }

        public void Dispose()
        {
            this.transport.Disconnect();
        }

        public virtual async Task Connect()
        {
            await this.transport.Connect().ConfigureAwait(false);
        }

        public virtual async Task Disconnect()
        {
            await this.transport.Disconnect().ConfigureAwait(false);
        }

        public virtual async Task SendFrame(byte[] data)
        {
            await this.SendFrameMulti(data).ConfigureAwait(false);
        }

        public virtual async Task SendFrameMulti(params byte[][] datae)
        {
            await this.framer.WriteFrame(this.transport, datae).ConfigureAwait(false);
        }

        public virtual async Task<byte[]> ReceiveFrame()
        {
            return await this.framer.ReadFrame(this.transport).ConfigureAwait(false);
        }

        public virtual async Task SendResponse(RPCResponse response)
        {
            string message = JsonConvert.SerializeObject(response);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            await this.SendFrame(data).ConfigureAwait(false);
        }

        public virtual async Task<RPCResponse> ReceiveResponse()
        {
            byte[] data = await this.ReceiveFrame().ConfigureAwait(false);
            string message = System.Text.UTF8Encoding.UTF8.GetString(data);
            return JsonConvert.DeserializeObject<RPCResponse>(message);
        }

        public virtual async Task SendRequest(RPCRequest request)
        {
            string message = JsonConvert.SerializeObject(request);
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(message);
            await this.SendFrame(data).ConfigureAwait(false);
        }

        public virtual async Task<RPCRequest> ReceiveRequest()
        {
            byte[] data = await this.ReceiveFrame().ConfigureAwait(false);
            string message = System.Text.UTF8Encoding.UTF8.GetString(data);
            return JsonConvert.DeserializeObject<RPCRequest>(message);
        }

        protected Task ProcessNextResponse()
        {
            // If the current task is unset/finished, go read a new response.
            if (this.nextResponseTask == null || this.nextResponseTask.IsCompleted)
            {
                this.nextResponseTask = this.ReceiveResponse().ContinueWith(responseTask =>
                {
                    foreach (RPCResult result in responseTask.Result)
                    {
                        if (this.resultBucket.ContainsKey(result.id))
                        {
                            throw new Exception(String.Format("Duplicate result with id {0}", result.id));
                        }
                        this.resultBucket[result.id] = result;
                    }
                });
            }
            // Otherwise just return the existing task.
            return this.nextResponseTask;
        }

        protected async Task<RPCResult> ReadResultWithId(int id)
        {
            RPCResult result;

            while (!this.resultBucket.ContainsKey(id))
            {
                await this.ProcessNextResponse().ConfigureAwait(false);
            }
            result = this.resultBucket[id];
            this.resultBucket.Remove(id);
            return result;
        }

        public virtual async Task<dynamic> InvokeRPCMethod(string service, string method, dynamic[] args, bool isNotification = false)
        {
            Random rnd = new Random();
            int callId = rnd.Next();
            RPCCall call = new RPCCall(service, method, args, callId);
            RPCRequest req = new RPCRequest { call };
            RPCResult result;

            await this.SendRequest(req).ConfigureAwait(false);

            if (isNotification)
            {
                return null;
            }

            result = await this.ReadResultWithId(callId).ConfigureAwait(false);
            if (result.error != null)
            {
                RPCError error = result.error;
                throw new RemoteError(error.message, error.code, error.data);
            }

            // Ignoring warnings for now, since C# doesn't really do those so much...
            return result.data;
        }
    }
}
