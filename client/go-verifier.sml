
(case CommandLine.arguments () of
    [first, last, logfile] =>
       Verifier.verifier (valOf (FromString.toInt first)) (valOf (FromString.toInt last)) logfile
  | _ =>
       print ("Usage: verifier first-block last-block log-file\n"))
;
