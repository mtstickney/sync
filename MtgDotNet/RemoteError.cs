using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet
{
    public class RemoteError : Exception
    {
        public string message;
        public int code;
        public dynamic data;

        public RemoteError(string message, int code, dynamic data = null)
        {
            this.message = message;
            this.code = code;
            this.data = data;
        }

        public override string ToString()
        {
            return String.Format("Remote Error ({0}): {1}", this.code, this.message);
        }
    }
}
