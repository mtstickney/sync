using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace MtgDotNet.Sys
{
    interface Transport
    {
        public async Task Connect();
        public async Task Disconnect();
        public async Task<byte[]> Read(uint size);
        public async Task ReadIntoArray(byte[] array, uint start, [Optional] uint? end);
        public async Task Write(byte[] data);
        public async Task Flush();
    }
}
