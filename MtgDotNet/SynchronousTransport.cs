using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace MtgDotNet.Sys
{
    public abstract class SynchronousTransport : ITransport
    {
        public abstract Task Connect();
        public abstract Task Disconnect();
        public abstract Task<byte[]> Read(uint size);
        public abstract Task ReadIntoArray(byte[] array, uint offset, [Optional] uint? size);
        public abstract Task Write(byte[] data);
        public abstract Task Flush();
    }
}
