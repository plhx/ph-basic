# pylint: disable=C0103, C0111
import ply.lex


class BLexer:
    reserved = {x.lower(): x for x in [
        'PRINT',
        'INPUT',
        'RETURN',
        'FUNC',
        'IF', 'THEN', 'ELSE',
        'WHILE', 'DO', 'BREAK',
        'END',
    ]}

    tokens = [
        'IDENT',
        'INTEGER',
        'ARRAY',

        'ASSIGN',
        'EQ', 'NE', 'LT', 'LE', 'GT', 'GE',
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO',
        'LPAREN', 'RPAREN',
        'COMMA',
    ] + list(reserved.values())

    t_ARRAY = r'@'

    t_ASSIGN = r'='
    t_EQ = r'=='
    t_NE = r'<>'
    t_LT = r'<'
    t_LE = r'<='
    t_GT = r'>'
    t_GE = r'>='
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_TIMES = r'\*'
    t_DIVIDE = r'/'
    t_MODULO = r'%'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_COMMA = r','
    t_ignore = ' \r\t'

    def t_IDENT(self, t):
        r'[A-Za-z_]\w*'
        t.type = self.reserved.get(t.value, 'IDENT')
        return t

    def t_INTEGER(self, t):
        r'-?\d+'
        t.value = int(t.value)
        return t

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_error(self, t):
        raise RuntimeError('illegal token {}'.format(t.value[0]))

    def __init__(self, **kwargs):
        self.lexer = ply.lex.lex(module=self, **kwargs)

    def lex(self, s):
        self.lexer.input(s)
        while True:
            token = self.lexer.token()
            if not token:
                raise StopIteration()
            yield token
