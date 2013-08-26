(* If you were using version 0.1, then you can refer to the modules the old way
   by opening this module *)
module Scgi = struct
  module Http_method = Http_method
  module Http_status = Http_status
  module Http_header = Http_header
  module Scgi_headers = Headers
  module Scgi_request = Request
  module Scgi_response = Response

  let handler = Server.handler_inet
end
