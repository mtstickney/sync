using System;
using System.Collections.Generic;
using System.Threading;

namespace NetstringPlus
{
    public class NetstringPlusAPI
    {
        public static void WriteNetstringBytes(System.IO.Stream stream, byte[] data)
        {
            byte[] bytes = NetstringBytes(data);
            stream.Write(bytes, 0, bytes.Length);
        }

        public static byte[] ReadNetstring(System.IO.Stream stream)
        {
            NetstringDecoder decoder = new NetstringDecoder(stream);
            return decoder.PumpMessage(); 
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
            return ReadNetstring(new System.IO.MemoryStream(nstring));
        }

        public static byte[] NetstringFromString(String str) {
            byte[] data = System.Text.UTF8Encoding.UTF8.GetBytes(str);
            return NetstringBytes(data);
        }
    }
}
