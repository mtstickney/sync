using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace MtgDotNet
{
    public class ClientNotAuthorizedException : Exception
    {
        public byte[] key { get; set; }

        public ClientNotAuthorizedException(byte[] key)
        {
            this.key = key;
        }

        public override string ToString()
        {
            return String.Format("Client with key {0} is not authorized", this.key);
        }
    }
}
