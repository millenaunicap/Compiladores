import ply.lex as lex

# Definição dos tokens
tokens = [
    'NUM_INT',
    'NUM_DEC',
    'ID',
    'TEXTO',
    'PALAVRA_RESERVADA',
    'COMENTARIO',
    'OPERADOR',
    'SIMBOLO_ESPECIAL'
]

# Palavras reservadas
reserved_words = {
    'int': 'PALAVRA_RESERVADA',
    'float': 'PALAVRA_RESERVADA',
    'char': 'PALAVRA_RESERVADA',
    'boolean': 'PALAVRA_RESERVADA',
    'void': 'PALAVRA_RESERVADA',
    'if': 'PALAVRA_RESERVADA',
    'else': 'PALAVRA_RESERVADA',
    'for': 'PALAVRA_RESERVADA',
    'while': 'PALAVRA_RESERVADA',
    'scanf': 'PALAVRA_RESERVADA',
    'println': 'PALAVRA_RESERVADA',
    'main': 'PALAVRA_RESERVADA',
    'return': 'PALAVRA_RESERVADA',
    'struct': 'PALAVRA_RESERVADA',
    'double': 'PALAVRA_RESERVADA',
    'long': 'PALAVRA_RESERVADA',
    'short': 'PALAVRA_RESERVADA',
    'unsigned': 'PALAVRA_RESERVADA',
    'signed': 'PALAVRA_RESERVADA',
    'const': 'PALAVRA_RESERVADA',
    'static': 'PALAVRA_RESERVADA',
    'switch': 'PALAVRA_RESERVADA',
    'case': 'PALAVRA_RESERVADA',
    'default': 'PALAVRA_RESERVADA',
    'break': 'PALAVRA_RESERVADA',
    'continue': 'PALAVRA_RESERVADA',
    'do': 'PALAVRA_RESERVADA',
    'goto': 'PALAVRA_RESERVADA',
    'sizeof': 'PALAVRA_RESERVADA',
    'typedef': 'PALAVRA_RESERVADA',
    'volatile': 'PALAVRA_RESERVADA',
    'enum': 'PALAVRA_RESERVADA',
    'extern': 'PALAVRA_RESERVADA',
    'register': 'PALAVRA_RESERVADA',
    'union': 'PALAVRA_RESERVADA'
}

# Dicionário para armazenar identificadores
identifiers = {}

# Regras de expressão regular para tokens simples
def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved_words.get(t.value, 'ID')  # Use o ID ou PALAVRA_RESERVADA conforme apropriado
    if t.type == 'ID':
        if t.value in identifiers:
            original_token = identifiers[t.value]
            t.lineno = original_token.lineno
            t.lexpos = original_token.lexpos
        else:
            identifiers[t.value] = t
    return t

def t_TEXTO(t):
    r'\".*?\"'
    return t

def t_COMENTARIO(t):
    r'//.*'
    return t

def t_OPERADOR(t):
    r'(\+|\-|\*|\/|\%|\&\&|\|\||\!|\>|\<|\>=|\<=|\!=|\==|\=)'
    return t

def t_SIMBOLO_ESPECIAL(t):
    r'[\(\)\[\]\{\}\,\;]'
    return t

# Ignorar espaços e tabulações
t_ignore = ' \t'

# Tratamento de erros
def t_error(t):
    print(f"Caractere ilegal: '{t.value[0]}'")
    t.lexer.skip(1)

# Construção do lexer
lexer = lex.lex()

# Entrada de exemplo
lexer.input("123 123.456 variavellll \"texto\" int if if variavellll 123 struct double // comentario")

# Impressão dos tokens
for token in lexer:
    print(f'Type: {token.type}, Value: {token.value}, Line: {token.lineno}, Position: {token.lexpos}')
