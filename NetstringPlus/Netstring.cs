using System;
using System.Collections.Generic;
using System.Threading;
using System.Linq;

namespace NetstringPlus
{
    public class NetstringPlusAPI
    {
        protected static byte[] NetstringHeader(uint size)
        {
            String len = String.Format("{0:X}:", size);
            return System.Text.UTF8Encoding.UTF8.GetBytes(len);
        }

        public static void WriteNetstringBytes(System.IO.Stream stream, byte[] data)
        {
            byte[] bytes = NetstringBytes(data);
            stream.Write(bytes, 0, bytes.Length);
        }

        public static void WriteNetstringBytesMulti(System.IO.Stream stream, IEnumerable<byte[]> datae)
        {
            // This kind of negates the performance benefits of having a *Multi method, maybe don't use an IEnumerable?
            List<byte[]> dataeList = new List<byte[]>();
            uint size;
            byte[] headerBytes;
            byte[] termBytes;

            foreach (byte[] data in datae)
            {
                dataeList.Add(data);
            }

            size = (uint)dataeList.Sum<byte[]>(x => x.Length);
            headerBytes = NetstringPlus.NetstringPlusAPI.NetstringHeader(size);
            termBytes = System.Text.UTF8Encoding.UTF8.GetBytes("\n");
            stream.Write(headerBytes, 0, headerBytes.Length);
            foreach (byte[] data in dataeList)
            {
                stream.Write(data, 0, data.Length);
            }
            stream.Write(termBytes, 0, termBytes.Length);
            return;
        }

        public static byte[] ReadNetstringData(System.IO.Stream stream)
        {
            NetstringDecoder decoder = new NetstringDecoder();
            List<byte[]> messages = decoder.PumpStream(stream, count: 1);
            if (messages.Count() < 1)
            {
                throw new System.IO.EndOfStreamException();
            }
            return messages[0];
        }

        public static byte[] NetstringBytes(byte[] data)
        {
            String len = String.Format("{0:X}", data.Length);

            byte[] headerBytes = System.Text.UTF8Encoding.UTF8.GetBytes(len);
            byte[] sepBytes = System.Text.UTF8Encoding.UTF8.GetBytes(":");
            byte[] termBytes = System.Text.UTF8Encoding.UTF8.GetBytes("\n");
            byte[] result = new byte[headerBytes.Length + sepBytes.Length + data.Length + termBytes.Length];
            int offset = 0;
            System.Buffer.BlockCopy(headerBytes, 0, result, offset, headerBytes.Length);
            offset += headerBytes.Length;
            System.Buffer.BlockCopy(sepBytes, 0, result, offset, sepBytes.Length);
            offset += sepBytes.Length;
            System.Buffer.BlockCopy(data, 0, result, offset, data.Length);
            offset += data.Length;
            System.Buffer.BlockCopy(termBytes, 0, result, offset, termBytes.Length);
            return result;
        }

        public static byte[] NetstringData(byte[] nstring)
        {
            using (System.IO.MemoryStream stream = new System.IO.MemoryStream(nstring))
            {
                return ReadNetstringData(new System.IO.MemoryStream(nstring));
            }
        }

        public static byte[] NetstringFromString(String str) {
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(str);
            return NetstringBytes(data);
        }
    }
}
