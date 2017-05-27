(ns vm-translator.startup)

(defn assign-number-to-segment [number segment]
  [(str "@" number) "D=A" (str "@" segment) "M=D"])

(def startup-code
  (concat
   (assign-number-to-segment 256 "SP")
   (assign-number-to-segment 300 "LCL")
   (assign-number-to-segment 400 "ARG")
   (assign-number-to-segment 3000 "THIS")
   (assign-number-to-segment 3010 "THAT")
   ["@Sys.init" "0;JMP"]))
