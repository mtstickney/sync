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
        public int id { get; set; }

        public RPCResult(dynamic data, RPCError error, RPCError[] warnings, int id)
        {
            this.data = data;
            this.error = error;
            this.warnings = warnings;
            this.id = id;
        }

        public RPCResult() { }
    }
}
