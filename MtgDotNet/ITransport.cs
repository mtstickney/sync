using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace MtgDotNet.Sys
{
    interface ITransport
    {
        Task Connect();
        Task Disconnect();
        Task<byte[]> Read(uint size);
        Task ReadIntoArray(byte[] array, uint offset, [Optional] uint? size);
        Task Write(byte[] data);
        Task Flush();
    }
}
