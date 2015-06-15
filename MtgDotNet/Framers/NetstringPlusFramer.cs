using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NetstringPlus;

namespace MtgDotNet.Framers
{
    public class NetstringPlusFramer : MtgDotNet.Sys.IFramer
    {
        public byte[] FrameData(byte[] data)
        {
            return NetstringPlusAPI.NetstringBytes(data);
        }

        public byte[] FrameDataMulti(params byte[][] datae)
        {
            using (System.IO.MemoryStream stream = new System.IO.MemoryStream())
            {
                NetstringPlusAPI.WriteNetstringBytesMulti(stream, datae);
                return stream.ToArray();
            }
        }

        public byte[] UnframeData(byte[] frame)
        {
            return NetstringPlusAPI.NetstringData(frame);
        }

        public async Task WriteFrame(MtgDotNet.Sys.ITransport t, params byte[][] datae)
        {
            byte[] frame = this.FrameDataMulti(datae);
            await t.Write(frame);
            return;
        }

        public async Task<byte[]> ReadFrame(MtgDotNet.Sys.ITransport t)
        {
            NetstringPlus.NetstringDecoder decoder = new NetstringPlus.NetstringDecoder();
            while (decoder.state != NetstringPlus.NetstringDecoder.DecoderState.COMPLETE)
            {
                // TODO: it would be more efficient to re-use the same array for e.g. the header digits, but that would violate encapsulation.
                byte[] data = await t.Read(decoder.NextReadSize());
                decoder.PumpArray(data, count: 1);
            }
            return decoder.GetData();
        }
    }
}
