module Stack_trace : sig
  type stack_frame = {
    filename : string option;
    function_ : string option;
    lineno : int option;
    colno : int option;
  }
  [@@deriving to_yojson, make]

  type t = stack_frame list [@@deriving to_yojson]

  val make_stacktrace : Printexc.raw_backtrace -> t
  (** Constructs a [list] of [stack_frame]s from an exception backtrace *)
end

module Exception : sig
  type t = { message : string; type_ : string; stacktrace : Stack_trace.t }
  [@@deriving to_yojson, make]

  val of_exn : Printexc.raw_backtrace -> exn -> t
  (** Constructs an [Exception.t] from an exception backtrace and [exception] *)
end

type t = {
  id : string;
  timestamp : int;
  trace_id : string option;
  transaction_id : string option;
  parent_id : string option;
  exception_ : Exception.t;
}
[@@deriving to_yojson, make]

val to_message_yojson : t -> Yojson.Safe.t

type parent =
  [ `Span of Span.t | `Transaction of Transaction.t | `Trace of Trace.t ]
(** An error can occur in a span, transaction, or trace *)

val send : t -> unit
(** Sends an error to the APM server *)

val of_result :
  ?parent:parent -> pp:'err Fmt.t -> ('ok, 'err) result -> t option
(** Constructs an error from a [result]. [parent] is an optional parent and [pp]
    is a formatter of the [Error v]. If the result is [Ok _], [None] is
    returned. *)

val of_exn : ?parent:parent -> Printexc.raw_backtrace -> exn -> t
(** Constructs an error from an exception backtrace and [exception] *)
