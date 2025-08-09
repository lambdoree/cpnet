(system DeclarativeSourceSystem
  (interface SourceTestInterface)
  (subsystems
    (source-impl apply-gate))
  (apply source-impl
    (code const-42)
    (args)
    (results (SourceTestInterface result))))
