
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMENTARIO ID NUM_DEC NUM_INT OPERADOR PALAVRA_RESERVADA SIMBOLO_ESPECIAL TEXTODeclaracaoVariavel : Tipo ID SIMBOLO_ESPECIAL\n                          | Tipo ID SIMBOLO_ESPECIAL OPERADOR Expressao SIMBOLO_ESPECIALTipo : PALAVRA_RESERVADA\n        Expressao : ID\n                 | NUM_INT\n                 | NUM_DEC'
    
_lr_action_items = {'PALAVRA_RESERVADA':([0,],[3,]),'$end':([1,5,11,],[0,-1,-2,]),'ID':([2,3,6,],[4,-3,7,]),'SIMBOLO_ESPECIAL':([4,7,8,9,10,],[5,-4,11,-5,-6,]),'OPERADOR':([5,],[6,]),'NUM_INT':([6,],[9,]),'NUM_DEC':([6,],[10,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'DeclaracaoVariavel':([0,],[1,]),'Tipo':([0,],[2,]),'Expressao':([6,],[8,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> DeclaracaoVariavel","S'",1,None,None,None),
  ('DeclaracaoVariavel -> Tipo ID SIMBOLO_ESPECIAL','DeclaracaoVariavel',3,'p_DeclaracaoVariavel','Teste.py',72),
  ('DeclaracaoVariavel -> Tipo ID SIMBOLO_ESPECIAL OPERADOR Expressao SIMBOLO_ESPECIAL','DeclaracaoVariavel',6,'p_DeclaracaoVariavel','Teste.py',73),
  ('Tipo -> PALAVRA_RESERVADA','Tipo',1,'p_Tipo','Teste.py',77),
  ('Expressao -> ID','Expressao',1,'p_Expressao','Teste.py',86),
  ('Expressao -> NUM_INT','Expressao',1,'p_Expressao','Teste.py',87),
  ('Expressao -> NUM_DEC','Expressao',1,'p_Expressao','Teste.py',88),
]
