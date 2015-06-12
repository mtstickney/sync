using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet.ProtocolObjects
{
    class RPCCall
    {
        public string service { get; set; }
        public string method { get; set; }
        public dynamic[] args { get; set; }
        public dynamic id { get; set; }
    }
}
