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
        public async Task Connect();
        public async Task Disconnect();
        public async Task<byte[]> Read(uint size);
        public async Task ReadIntoArray(byte[] array, uint offset, [Optional] uint? size);
        public async Task Write(byte[] data);
        public async Task Flush();
    }
}
