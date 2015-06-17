using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MtgDotNet
{
    public abstract class MtgDotNetAPI
    {
        public static MtgDotNet.IRPCConnection MakeConnection(string host, int port)
        {
            MtgDotNet.Sys.IFramer framer = (MtgDotNet.Sys.IFramer)new MtgDotNet.Framers.NetstringPlusFramer();
            MtgDotNet.Sys.ITransport transport = (MtgDotNet.Sys.ITransport)new MtgDotNet.Transports.AsynchronousTcpTransport(host, port);
            MtgDotNet.IRPCConnection con = (MtgDotNet.IRPCConnection)new MtgDotNet.RPCConnection(framer, transport);
            return con;
        }

        public static MtgDotNet.IRPCConnection MakeConnection(System.Net.Sockets.TcpClient socket)
        {
            MtgDotNet.Sys.IFramer framer = (MtgDotNet.Sys.IFramer)new MtgDotNet.Framers.NetstringPlusFramer();
            MtgDotNet.Sys.ITransport transport = (MtgDotNet.Sys.ITransport)new MtgDotNet.Transports.AsynchronousTcpTransport(socket);
            MtgDotNet.IRPCConnection con = (MtgDotNet.IRPCConnection)new MtgDotNet.RPCConnection(framer, transport);
            return con;
        }

        public static MtgDotNet.IRPCConnection MakeEncryptedConnection(string host, int port, byte[] secret, byte[][] authorizedKeys)
        {
            MtgDotNet.Sys.IFramer framer = (MtgDotNet.Sys.IFramer)new MtgDotNet.Framers.NetstringPlusFramer();
            MtgDotNet.Sys.ITransport transport = (MtgDotNet.Sys.ITransport)new MtgDotNet.Transports.AsynchronousTcpTransport(host, port);
            MtgDotNet.IRPCConnection con = (MtgDotNet.IRPCConnection)new MtgDotNet.EncryptedRPCConnection(framer, transport, secret, authorizedKeys);
            return con;
        }

        public static MtgDotNet.IRPCConnection MakeEncryptedConnection(System.Net.Sockets.TcpClient socket, byte[] secret, byte[][] authorizedKeys)
        {
            MtgDotNet.Sys.IFramer framer = (MtgDotNet.Sys.IFramer)new MtgDotNet.Framers.NetstringPlusFramer();
            MtgDotNet.Sys.ITransport transport = (MtgDotNet.Sys.ITransport)new MtgDotNet.Transports.AsynchronousTcpTransport(socket);
            MtgDotNet.IRPCConnection con = (MtgDotNet.IRPCConnection)new MtgDotNet.EncryptedRPCConnection(framer, transport, secret, authorizedKeys);
            return con;
        }
    }
}
