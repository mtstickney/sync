using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MtgDotNet.Sys
{
    public interface IFramer
    {
        byte[] FrameData(byte[] data);
        byte[] FrameDataMulti(IEnumerable<byte[]> datae);
        byte[] UnframeData(byte[] frame);
        Task WriteFrame(ITransport t, IEnumerable<byte[]> datae);
        Task<byte[]> ReadFrame(ITransport t);
    }
}
