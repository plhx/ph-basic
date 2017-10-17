# pylint: disable=C0103, C0111
import ast
import ply.yacc
from blexer import BLexer


class BParser:
    precedence = [
        ('left', 'EQ', 'NE', 'LT', 'LE', 'GT', 'GE'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE', 'MODULO'),
    ]

    binop = {
        '+': ast.Add, '-': ast.Sub, '*': ast.Mult, '/': ast.FloorDiv, '%': ast.Mod,
        '==': ast.Eq, '<>': ast.NotEq,
        '<': ast.Lt, '<=': ast.LtE, '>': ast.Gt, '>=': ast.GtE,
    }

    def p_prog(self, p):
        '''prog : stmt_list'''
        p[0] = ast.Module(
            body=ast.parse('__array__ = {}').body + p[1],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_funcdef_vars(self, p):
        '''funcdef_vars : funcdef_vars_item
                        | '''
        p[0] = [] if len(p) == 1 else p[1]

    def p_funcdef_vars_item(self, p):
        '''funcdef_vars_item : funcdef_vars_item COMMA IDENT
                             | IDENT'''
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

    def p_funcdef(self, p):
        'funcdef : FUNC IDENT LPAREN funcdef_vars RPAREN stmt_list END'
        p[0] = ast.FunctionDef(
            name=p[2],
            args=ast.arguments(
                args=[
                    ast.arg(
                        arg=x, annotation=None,
                        lineno=p.lineno(1), col_offset=p.lexpos(1)
                    ) for x in p[4]
                ],
                vararg=None, kwonlyargs=[], kw_defaults=[], kwarg=None, defaults=[]
            ),
            body=p[6] + [
                ast.Return(
                    value=ast.Num(
                        n=0,
                        lineno=p.lineno(1), col_offset=p.lexpos(1)
                    ),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            decorator_list=[],
            returns=None,
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_list(self, p):
        '''stmt_list : stmt stmt_list
                     | funcdef stmt_list
                     | '''
        p[0] = [p[1]] + p[2] if len(p) == 3 else []

    def p_stmt_break(self, p):
        'stmt : BREAK'
        p[0] = ast.Break(lineno=p.lineno(1), col_offset=p.lexpos(1))

    def p_stmt_return(self, p):
        'stmt : RETURN expr'
        p[0] = ast.Return(
            value=p[2],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_assign_scalar(self, p):
        'stmt : IDENT ASSIGN expr'
        if len(p[2]) != 1:
            self.p_error(p)
        p[0] = ast.Assign(
            targets=[
                ast.Name(
                    id=p[1], ctx=ast.Store(),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            value=p[3],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_assign_array(self, p):
        'stmt : ARRAY LPAREN expr RPAREN ASSIGN expr'
        p[0] = ast.Assign(
            targets=[
                ast.Subscript(
                    value=ast.Name(
                        id='__array__', ctx=ast.Load(),
                        lineno=p.lineno(1), col_offset=p.lexpos(1)
                    ),
                    slice=ast.Index(
                        value=p[3],
                        lineno=p.lineno(1), col_offset=p.lexpos(1)
                    ),
                    ctx=ast.Store(),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            value=p[6],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_if(self, p):
        'stmt : IF expr THEN stmt_list END'
        p[0] = ast.If(
            test=p[2], body=p[4], orelse=[],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_ifelse(self, p):
        'stmt : IF expr THEN stmt_list ELSE stmt_list END'
        p[0] = ast.If(
            test=p[2], body=p[4], orelse=p[6],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_while(self, p):
        'stmt : WHILE expr DO stmt_list END'
        p[0] = ast.While(
            test=p[2], body=p[4], orelse=[],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_print(self, p):
        'stmt : PRINT expr'
        p[0] = ast.Expr(
            value=ast.Call(
                func=ast.Name(
                    id='print', ctx=ast.Load(),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                ),
                args=[p[2]],
                keywords=[],
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_input_scalar(self, p):
        'stmt : INPUT IDENT'
        if len(p[2]) != 1:
            self.p_error(p)
        p[0] = ast.Assign(
            targets=[
                ast.Name(
                    id=p[2], ctx=ast.Store(),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            value=ast.Call(
                func=ast.Name(
                    id='int', ctx=ast.Load(),
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                ),
                args=[
                    ast.Call(
                        func=ast.Name(
                            id='input', ctx=ast.Load(),
                            lineno=p.lineno(1), col_offset=p.lexpos(1)
                        ),
                        args=[
                            ast.Str(s='? ', lineno=p.lineno(1), col_offset=p.lexpos(1))
                        ], keywords=[],
                        lineno=p.lineno(1), col_offset=p.lexpos(1)
                    )
                ], keywords=[],
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_stmt_expr(self, p):
        'stmt : expr'
        p[0] = ast.Expr(value=p[1], lineno=p.lineno(1), col_offset=p.lexpos(1))

    def p_expr_list(self, p):
        '''expr_list : expr_item
                     | '''
        p[0] = [] if len(p) == 1 else p[1]

    def p_expr_item(self, p):
        '''expr_item : expr_item COMMA expr
                     | expr'''
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

    def p_expr_binop(self, p):
        '''expr : expr PLUS expr
                | expr MINUS expr
                | expr TIMES expr
                | expr DIVIDE expr
                | expr MODULO expr'''
        p[0] = ast.Call(
            func=ast.Name(
                id='int', ctx=ast.Load(),
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            args=[
                ast.BinOp(
                    left=p[1], op=self.binop[p[2]](), right=p[3],
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            keywords=[],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_expr_cmp(self, p):
        '''expr : expr EQ expr
                | expr NE expr
                | expr LT expr
                | expr LE expr
                | expr GT expr
                | expr GE expr'''
        p[0] = ast.Call(
            func=ast.Name(
                id='int', ctx=ast.Load(),
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            args=[
                ast.Compare(
                    left=p[1], ops=[self.binop[p[2]]()], comparators=[p[3]],
                    lineno=p.lineno(1), col_offset=p.lexpos(1)
                )
            ],
            keywords=[],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_expr_int(self, p):
        'expr : INTEGER'
        p[0] = ast.Num(n=p[1], lineno=p.lineno(1), col_offset=p.lexpos(1))

    def p_expr_paren(self, p):
        'expr : LPAREN expr RPAREN'
        p[0] = p[2]

    def p_term_scalar(self, p):
        'expr : IDENT'
        if len(p[1]) != 1:
            self.p_error(p)
        p[0] = ast.Name(
            id=p[1], ctx=ast.Load(),
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_expr_array(self, p):
        'expr : ARRAY LPAREN expr RPAREN'
        p[0] = ast.Subscript(
            value=ast.Name(
                id='__array__', ctx=ast.Load(),
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            slice=ast.Index(
                value=p[3],
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            ctx=ast.Load(),
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_expr_call(self, p):
        'expr : IDENT LPAREN expr_list RPAREN'
        p[0] = ast.Call(
            func=ast.Name(
                id=p[1], ctx=ast.Load(),
                lineno=p.lineno(1), col_offset=p.lexpos(1)
            ),
            args=p[3], keywords=[],
            lineno=p.lineno(1), col_offset=p.lexpos(1)
        )

    def p_error(self, p):
        raise RuntimeError(p)

    def __init__(self, lexer = BLexer(), **kwargs):
        self.lexer = lexer
        self.tokens = lexer.tokens
        self.parser = ply.yacc.yacc(module=self, **kwargs)

    def execute(self, s, **kwargs):
        a = self.parser.parse(s, **kwargs)
        c = compile(a, '<string>', 'exec')
        exec(c)


"""
BParser().execute('''

func is_prime(n)
    if n < 2 then return 0 end
    if n == 2 then return 1 end
    if n % 2 == 0 then return 0 end
    i = 3
    while i * i <= n do
        if n % i == 0 then return 0 end
        i = i + 1
    end
    return 1
end

i = 2
while i < 30 do
    if is_prime(i) then
        print i
    end
    i = i + 1
end

''')
"""
