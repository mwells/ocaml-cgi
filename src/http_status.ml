(** Non-comprehensive list of HTTP response status codes and strings from
    http://en.wikipedia.org/wiki/List_of_HTTP_status_codes *)
type t =
    [ `Ok
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
    | `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Temporary_redirect
    | `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Request_entity_too_large
    | `Request_uri_too_long
    | `Unsupported_media_type
    | `Requested_range_not_satisfiable
    | `Expectation_failed
    | `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    | `Custom_code of (int * string)
    ]

let values = function
  | `Ok                              -> 200, "OK"
  | `Created                         -> 201, "Created"
  | `Accepted                        -> 202, "Accepted"
  | `Non_authoritative_information   -> 203, "Non-Authoritative Information"
  | `No_content                      -> 204, "No Content"
  | `Reset_content                   -> 205, "Reset Content"
  | `Partial_content                 -> 206, "Partial Content"
  | `Multiple_choices                -> 300, "Multiple Choices"
  | `Moved_permanently               -> 301, "Moved Permanently"
  | `Found                           -> 302, "Found"
  | `See_other                       -> 303, "See Other"
  | `Not_modified                    -> 304, "Not Modified"
  | `Temporary_redirect              -> 307, "Temporary Redirect"
  | `Bad_request                     -> 400, "Bad Request"
  | `Unauthorized                    -> 401, "Unauthorized"
  | `Payment_required                -> 402, "Payment Required"
  | `Forbidden                       -> 403, "Forbidden"
  | `Not_found                       -> 404, "Not Found"
  | `Method_not_allowed              -> 405, "Method Not Allowed"
  | `Not_acceptable                  -> 406, "Not Acceptable"
  | `Proxy_authentication_required   -> 407, "Proxy Authentication Required"
  | `Request_timeout                 -> 408, "Request Timeout"
  | `Conflict                        -> 409, "Conflict"
  | `Gone                            -> 410, "Gone"
  | `Length_required                 -> 411, "Length Required"
  | `Precondition_failed             -> 412, "Precondition Failed"
  | `Request_entity_too_large        -> 413, "Request Entity Too Large"
  | `Request_uri_too_long            -> 414, "Request URI Too Long"
  | `Unsupported_media_type          -> 415, "Unsupported Media Type"
  | `Requested_range_not_satisfiable -> 416, "Request Range Not Satisfiable"
  | `Expectation_failed              -> 417, "Expectation Failed"
  | `Internal_server_error           -> 500, "Internal Server Error"
  | `Not_implemented                 -> 501, "Not Implemented"
  | `Bad_gateway                     -> 502, "Bad Gateway"
  | `Service_unavailable             -> 503, "Service Unavailable"
  | `Gateway_timeout                 -> 504, "Gateway Timeout"
  | `Http_version_not_supported      -> 505, "HTTP Version Not Supported"
  | `Custom_code (code, name)        -> code, name

let to_int    v = fst (values v)
let to_string v = snd (values v)
