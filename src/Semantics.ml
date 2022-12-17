open Expression
open Configuration
let plus { expression = (Op(Integer(x), Integer(y))); store = s } = 
  {expression = Integer(x + y); store = s}

let geq { expression = (Op(Integer(x), Integer(y))); store = s } = 
  {expression = Boolean(x >= y); store = s}