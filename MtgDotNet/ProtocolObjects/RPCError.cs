using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet.ProtocolObjects
{
    public class RPCError
    {
        public string message { get; set; }
        public int code { get; set; }
        public dynamic data { get; set; }
    }
}
