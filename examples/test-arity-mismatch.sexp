(system ArityMismatchSystem
  (interface SourceTestInterface)
  (subsystems
    (source-impl apply-gate))
  (apply source-impl
    (code const-42)
    ;; Mismatch: const-42 takes 0 args, but we provide 1.
    (args (SourceTestInterface result))
    ;; Mismatch: const-42 produces 1 result, but we provide 0.
    (results)))
