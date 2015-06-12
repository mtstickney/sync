using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MtgDotNet.Sys
{
    interface Framer
    {
        public byte[] FrameData(byte[] data);
        public byte[] FrameDataMulti(IEnumerable<byte[]> datae);
        public byte[] UnframeData(byte[] frame);
        public async Task WriteFrame(Transport t, IEnumerable<byte[]> datae);
        public async Task<byte[]> ReadFrame(Transport t);
    }
}
