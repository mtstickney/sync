using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net;
using System.Net.Sockets;

namespace MtgDotNet.Transports
{
    public class AsynchronousTcpTransport : MtgDotNet.Sys.AsynchronousTransport, IDisposable
    {
        protected string address;
        protected int port;
        protected TcpClient socket = null;

        public AsynchronousTcpTransport(string address, int port)
        {
            this.address = address;
            this.port = port;
        }

        public AsynchronousTcpTransport(TcpClient socket)
        {
            if (!socket.Connected)
            {
                throw new Exception("Cannot create a transport from a disconnected socket. Use the host/port constructor instead.");
            }

            IPEndPoint endpoint = (IPEndPoint)this.socket.Client.RemoteEndPoint;
            this.address = endpoint.Address.ToString();
            this.port = endpoint.Port;
            this.socket = socket;
        }

        public void Dispose()
        {
            if (this.socket != null)
            {
                this.socket.Close();
            }
        }

        public async override Task Connect()
        {
            if (this.socket != null)
            {
                await this.Disconnect().ConfigureAwait(false);
            }
            this.socket = new TcpClient();
            this.socket.Connect(this.address, this.port);
        }

        public async override Task Disconnect()
        {
            if (this.socket != null && this.socket.Connected)
            {
                this.socket.Close();
            }

            if (this.socket != null)
            {
                this.socket = null;
            }
        }

        public async override Task ReadIntoArray(byte[] array, uint offset, uint? size = null)
        {
            int bytes = 0;

            NetworkStream stream = this.socket.GetStream();
            if (size == null)
            {
                size = (uint)array.Length;
            }

            while (bytes < size)
            {
                // FIXME: these casts can go bad, do it in multiple parts if necessary.
                bytes += await stream.ReadAsync(array, (int)offset + bytes, (int)size - bytes).ConfigureAwait(false);
            }

            return;
        }

        public async override Task<byte[]> Read(uint size)
        {
            byte[] buf = new byte[size];
            await this.ReadIntoArray(buf, 0, size).ConfigureAwait(false);
            return buf;
        }

        public async override Task Write(byte[] data)
        {
            NetworkStream stream = this.socket.GetStream();
            await stream.WriteAsync(data, 0, data.Length).ConfigureAwait(false);
            return;
        }

        public async override Task Flush()
        {
            NetworkStream stream = this.socket.GetStream();
            await stream.FlushAsync().ConfigureAwait(false);
            return;
        }
    }
}
