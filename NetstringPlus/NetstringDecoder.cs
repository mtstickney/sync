using System;
using System.Collections.Generic;

namespace NetstringPlus
{
    public class NetstringDecoder
    {
        public enum DecoderState { INITIAL, HEADER, DATA, ENDOFDATA, COMPLETE };

        public DecoderState state { get; set; }
        public int dataSize { get; set; }
        public byte[] data { get; set; }
        public int dataPos { get; set; }

        public NetstringDecoder() {
            this.Reset();
        }

        public void Reset()
        {
            this.state = DecoderState.INITIAL;
            this.dataSize = 0;
            this.dataPos = 0;
            this.data = null;
        }

        public byte[] GetData()
        {
            return this.data;
        }

        public static int AsciiHexDigit(byte c)
        {
            if ('0' <= c && c <= '9')
            {
                return c - '0';
            }
            else if ('A' <= c && c <= 'F')
            {
                return c - 'A';
            }
            else if ('a' <= c && c <= 'f')
            {
                return c - 'a';
            }
            else
            {
                return -1;
            }
        }

        public void AddHeaderDigit(int digit) {
            this.dataSize = this.dataSize * 16 + digit;
        }

        public void TransitionState(DecoderState toState) {
            switch(toState) {
                case DecoderState.INITIAL:
                    // FIXME: throw error here
                    break;
                case DecoderState.HEADER:
                    if (this.state != DecoderState.INITIAL) {
                        // FIXME: throw error here
                    }
                    break;
                case DecoderState.DATA:
                    if (this.state != DecoderState.HEADER) {
                        // FIXME: throw error here
                    }
                    break;
                case DecoderState.ENDOFDATA:
                    if (this.state != DecoderState.DATA && this.state != DecoderState.HEADER) {
                        // FIXME: throw error here
                    }
                    break;
                case DecoderState.COMPLETE:
                    if (this.state != DecoderState.ENDOFDATA) {
                        // FIXME: throw error here
                    }
                    break;
            }
            this.state = toState;
        }

        public uint NextReadSize() {
            switch (this.state)
            {
                case DecoderState.INITIAL:
                case DecoderState.HEADER:
                    return 1;
                case DecoderState.ENDOFDATA:
                    return 1;
                case DecoderState.DATA:
                    // dataSize and dataPos will always be positive, and dataPos <= dataSize.
                    return (uint)(this.dataSize - this.dataPos);
                case DecoderState.COMPLETE:
                    return 0;
                default:
                    throw new Exception(String.Format("Invalid decoder state {0}", this.state));
            }
        }

        public void PumpByte(byte b) {
            switch(this.state) {
                case DecoderState.INITIAL:
                case DecoderState.HEADER:
                    {
                        int n = NetstringDecoder.AsciiHexDigit((byte)b);
                        if (b == ':')
                        {
                            if (this.state == DecoderState.INITIAL)
                            {
                                throw new Exception("Netstring header is empty");
                            }
                            else if (this.dataSize == 0)
                            {
                                this.dataPos = 0;
                                this.TransitionState(DecoderState.ENDOFDATA);
                            }
                            else
                            {
                                this.dataPos = 0;
                                this.data = new byte[this.dataSize];
                                this.TransitionState(DecoderState.DATA);
                            }
                        }
                        else if (n < 0)
                        {
                            throw new Exception(String.Format("Invalid header character '{0}'", n));
                        }
                        else
                        {
                            this.AddHeaderDigit(n);
                            if (this.state == DecoderState.INITIAL)
                            {
                                this.TransitionState(DecoderState.HEADER);
                            }
                        }
                        break;
                    }
                case DecoderState.DATA:
                    {
                        this.data[this.dataPos++] = b;
                        if (this.dataPos == this.dataSize)
                        {
                            this.TransitionState(DecoderState.ENDOFDATA);
                        }
                        break;
                    }
                case DecoderState.ENDOFDATA:
                    {
                        if (b != '\n')
                        {
                            throw new Exception("Too much data in netstring");
                        }
                        else
                        {
                            this.TransitionState(DecoderState.COMPLETE);
                        }
                        break;
                    }
                case DecoderState.COMPLETE:
                    {
                        throw new Exception("Decoder is already complete");
                    }
            }
        }

        public List<byte[]> PumpStream(System.IO.Stream stream, uint? count = null, uint? bytes = null) {
            List<byte[]> messages = new List<Byte[]>();
            int i = 0;
            int byteCount = 0;

            // If the current decoder is already complete, reset the state.
            if (this.state == DecoderState.COMPLETE)
            {
                this.Reset();
            }

            try
            {
                while ((bytes == null || byteCount < bytes) && (count == null || i < count))
                {
                    int b = stream.ReadByte();
                    if (b < 0)
                    {
                        // End of stream, return the messages we've read so far.
                        return messages;
                    }

                    this.PumpByte((byte)b);
                    byteCount++;

                    if (this.state == DecoderState.COMPLETE)
                    {
                        messages.Add(this.data);
                        i++;
                        this.Reset();
                    }
                }
            }
            catch (System.IO.EndOfStreamException)
            {
                // Just swallow it.
            }
            return messages;
        }

        public List<byte[]> PumpArray(byte[] array, uint start = 0, uint? end = null, uint? count = null)
        {
            List<byte[]> messages = new List<byte[]>();
            uint i = 0;

            if (end == null)
            {
                end = (uint)array.Length;
            }

            while (start < end && (count == null || i < count))
            {
                byte b = array[start];

                // If the decoder is already complete, get a fresh state.
                if (this.state == DecoderState.COMPLETE)
                {
                    this.Reset();
                }

                this.PumpByte(b);
                start++;

                if (this.state == DecoderState.COMPLETE)
                {
                    messages.Add(this.data);
                    i++;
                }
            }
            return messages;
        }
    }
}
