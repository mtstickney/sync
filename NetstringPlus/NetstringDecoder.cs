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
        private System.IO.Stream byteStream;
        private System.IO.BinaryReader charStream;

        public static NetstringDecoder NewDecoderForStream(System.IO.Stream stream) {
            return new NetstringDecoder(stream);
        }

        public NetstringDecoder(System.IO.Stream stream) {
            this.state = DecoderState.INITIAL;
            this.byteStream = stream;
            this.charStream = new System.IO.BinaryReader(stream, System.Text.Encoding.UTF8);
        }

        public static int HexDigit(char c) {
            int i = "0123456789ABCDEFabcdef".IndexOf(c);
            if (i > 15) {
                return i - 6;
            }
            return i;
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

        public void PumpElement() {
            switch(this.state) {
                case DecoderState.INITIAL:
                case DecoderState.HEADER:
                    {
                        int c = this.charStream.ReadChar();
                        if (c < 0)
                        {
                            throw new System.IO.EndOfStreamException();
                        }

                        int n = NetstringDecoder.HexDigit((char)c);
                        if (c == ':')
                        {
                            if (this.state == DecoderState.INITIAL)
                            {
                                // FIXME: throw empty-header error here
                            }
                            else if (this.dataSize == 0)
                            {
                                this.TransitionState(DecoderState.ENDOFDATA);
                            }
                            else
                            {
                                this.data = new byte[this.dataSize];
                                this.TransitionState(DecoderState.DATA);
                            }
                        }
                        else if (n < 0)
                        {
                            // FIXME: throw invalid header character error
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
                        int count = this.byteStream.Read(this.data, 0, this.dataSize);
                        if (count < this.dataSize)
                        {
                            String msg = String.Format("Only able to read {0} bytes of data (wanted {1})", count, this.dataSize);
                            throw new System.IO.EndOfStreamException(msg);
                        }
                        this.TransitionState(DecoderState.ENDOFDATA);
                        break;
                    }
                case DecoderState.ENDOFDATA:
                    {
                        int c = this.charStream.ReadChar();
                        if (c < 0)
                        {
                            throw new System.IO.EndOfStreamException();
                        }
                        else if (c != '\n')
                        {
                            // FIXME: throw extra-data exception
                        }
                        else
                        {
                            this.TransitionState(DecoderState.COMPLETE);
                        }
                        break;
                    }
                case DecoderState.COMPLETE:
                    // FIXME: throw error because we're already done
                    break;
            }
        }

        public byte[] PumpMessage() {
            while (this.state != DecoderState.COMPLETE) {
                this.PumpElement();
            }
            return this.data;
        }

        // TODO: Add a PumpMessages(count) method, or replace this one (count == -1 => pump all).
        public List<byte[]> PumpAllMessages() {
            List<byte[]> messages = new List<byte[]>();
            while (true) {
                try {
                    byte[] message = this.PumpMessage();
                    messages.Add(message);
                } catch (System.IO.EndOfStreamException e) {
                    return messages;
                }
            }
        }

        public void Resynchronize() {
            // Attempt to resynchronize after error by discarding data until we read a newline.
            while (true) {
                int c = this.charStream.ReadChar();
                if (c == '\n') {
                    return;
                }
            }
        }
    }
}
