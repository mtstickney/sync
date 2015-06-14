using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet.ProtocolObjects
{
    public class RPCResult
    {
        public dynamic data { get; set; }
        public RPCError error { get; set; }
        public RPCError[] warnings { get; set; }
        public dynamic id { get; set; }
    }
}
