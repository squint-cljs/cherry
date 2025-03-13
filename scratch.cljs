(defprotocol IFoo (foo [_] "docstring"))
(extend-type string IFoo (foo [_] :foo))
(prn (foo "bar"))
