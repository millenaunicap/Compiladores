import ply.lex as lex

# Lista de tokens
tokens = ['INTEGER']

# Regras para tokens simples
t_INTEGER = r'\d+'

# Ignorar caracteres em branco
t_ignore = ' \t'

# Tratamento de erro
def t_error(t):
    print("Caractere ilegal: '%s'" % t.value[0])
    t.lexer.skip(1)

# Criando o lexer
lexer = lex.lex()

# Testando o lexer
lexer.input("123 456")

# Iterando sobre os tokens
for token in lexer:
    print(token)