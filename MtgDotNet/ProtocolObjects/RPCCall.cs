using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet.ProtocolObjects
{
    public class RPCCall
    {
        public string service { get; set; }
        public string method { get; set; }
        public dynamic[] args { get; set; }
        public int? id { get; set; }

        public RPCCall(string service, string method, dynamic[] args, int? id)
        {
            this.service = service;
            this.method = method;
            this.args = args;
            this.id = id;
        }

        public RPCCall() { }
    }
}
