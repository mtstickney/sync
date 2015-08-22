using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net.Sockets;
using System.Net;

namespace MtgDotNet.Transports
{
    public class SynchronousTcpTransport : MtgDotNet.Sys.SynchronousTransport, IDisposable
    {
        protected string address;
        protected int port;
        protected TcpClient socket = null;

        public SynchronousTcpTransport(string address, int port)
        {
            this.address = address;
            this.port = port;
        }

        public SynchronousTcpTransport(TcpClient socket)
        {
            if (!socket.Connected)
            {
                throw new Exception("Cannot instantiate a transport with a closed socket. Use the hostname/port constructor instead.");
            }

            // This is a bit of a cheat, since we may be using an IP address where the original socket was connected via hostname.
            IPEndPoint endpoint = (IPEndPoint)socket.Client.RemoteEndPoint;
            this.port = endpoint.Port;
            this.address = endpoint.Address.ToString();
            this.socket = socket;
        }

        public void Dispose()
        {
            if (this.socket != null)
            {
                this.socket.Close();
            }
        }

        public override Task Connect()
        {
            if (this.socket != null)
            {
                this.Disconnect();
            }

            this.socket = new TcpClient();
            this.socket.Connect(this.address, this.port);

            var tcs = new TaskCompletionSource<bool>();
            tcs.SetResult(true);
            return tcs.Task;
        }

        public override Task Disconnect()
        {
            if (this.socket != null && this.socket.Connected)
            {
                this.socket.Close();
            }

            if (this.socket != null)
            {
                this.socket = null;
            }
            var tcs = new TaskCompletionSource<bool>();
            tcs.SetResult(true);
            return tcs.Task;
        }

        public override Task ReadIntoArray(byte[] array, uint offset, uint? size = null)
        {
            NetworkStream stream = this.socket.GetStream();
            int bytes;

            if (size == null)
            {
                size = (uint)array.Length;
            }

            // FIXME: this cast can go bad, do it in multiple parts if necessary.
            bytes = stream.Read(array, (int)offset, (int)size);
            if (bytes < size)
            {
                throw new System.IO.EndOfStreamException();
            }

            var tcs = new TaskCompletionSource<bool>();
            tcs.SetResult(true);
            return tcs.Task;
        }

        public override Task<byte[]> Read(uint size)
        {
            byte[] buf = new byte[size];

            this.ReadIntoArray(buf, 0, (uint)buf.Length);
            var tcs = new TaskCompletionSource<byte[]>();
            tcs.SetResult(buf);
            return tcs.Task;
        }

        public override Task Write(byte[] data)
        {
            NetworkStream stream = this.socket.GetStream();
            stream.Write(data, 0, data.Length);

            var tcs = new TaskCompletionSource<bool>();
            tcs.SetResult(true);
            return tcs.Task;
        }

        public override Task Flush()
        {
            this.socket.GetStream().Flush();
            var tcs = new TaskCompletionSource<bool>();
            tcs.SetResult(true);
            return tcs.Task;
        }

        void IDisposable.Dispose()
        {
            throw new NotImplementedException();
        }
    }
}
