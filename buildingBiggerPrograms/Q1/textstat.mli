type stats
val lines : stats -> int
val sentences : stats -> int
val words : stats -> int
val characters : stats -> int
val histogram : stats -> int array
val histogram_print : int array -> unit
val stats_from_file : string -> stats
