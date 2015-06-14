using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace MtgDotNet
{
    public interface IRPCConnection
    {
        Task Connect();
        Task Disconnect();
        Task<dynamic> InvokeRPCMethod(string service, string method, dynamic[] args, [Optional] bool isNotification);
    }
}
