#ifndef _LUA_GRAMMAR_HPP_INCLUDED_SOD8FU3498USFDLKJSFDLKJ4OUISDFLKJSLFKJASODIUJ4OIUJSFDKLJOIJSFDLKJSLKJDOFIJ4OIEWOIDKDKDF
#define _LUA_GRAMMAR_HPP_INCLUDED_SOD8FU3498USFDLKJSFDLKJ4OUISDFLKJSLFKJASODIUJ4OIUJSFDKLJOIJSFDLKJSLKJDOFIJ4OIEWOIDKDKDF

#define BOOST_SPIRIT_DEBUG
#include <boost/bind.hpp>
#include <boost/spirit/include/classic.hpp>
#include <iostream>
#include <stdexcept>

/* Lua 5.0 BNF:

This BNF is extracted from http://www.lua.org/manual/5.0/manual.html#BNF

<bnf>
chunk ::= {stat [';']}

block ::= chunk

stat ::=           varlist1 '=' explist1 |
                   functioncall | 
                   do block end | 
                   while exp do block end | 
                   repeat block until exp | 
                   if exp then block {elseif exp then block} [else block] end | 
                   return [explist1] | 
                   break | 
                   for Name '=' exp ',' exp [',' exp] do block end | 
                   for Name {',' Name} in explist1 do block end | 
                   function funcname funcbody | 
                   local function Name funcbody |
                   local namelist [init]

funcname ::= Name {'.' Name} [':' Name]

varlist1 ::= var {',' var}

var ::=  Name | prefixexp '[' exp ']' | prefixexp '.' Name

namelist ::= Name {',' Name}

init ::= '=' explist1

explist1 ::= {exp ','} exp

exp ::=  nil false true | Number | Literal | function | prefixexp | tableconstructor | exp binop exp | unop exp

prefixexp ::= var | functioncall | '(' exp ')'

functioncall ::=  prefixexp args | prefixexp ':' Name args

args ::=  '(' [explist1] ')' | tableconstructor | Literal

function ::= function funcbody

funcbody ::= '(' [parlist1] ')' block end

parlist1 ::=  Name {',' Name} [',' '...'] | '...'

tableconstructor ::= '{' [fieldlist] '}'
fieldlist ::= field {fieldsep field} [fieldsep]
field ::= '[' exp ']' '=' exp | name '=' exp | exp
fieldsep ::= ',' | ';'

binop ::= '+' | '-' | '*' | '/' | '^' | '..' | '<' | '<=' | '>' | '>=' | '==' | '~=' | and | or

unop ::= '-' | not
</bnf>

As one can see, there are some left recursion in this bnf: prefixexp, var, functioncall, exp.
I had to add several new 'specialized' rules to avoid this.

*/

#if 0
    Lua 5.1 BNF:

    chunk ::= block

    block ::= {stat} [laststat [¡®;¡¯]]

    stat ::=  ¡®;¡¯ | 
         varlist ¡®=¡¯ explist | 
         functioncall | 
         do block end | 
         while exp do block end | 
         repeat block until exp | 
         if exp then block {elseif exp then block} [else block] end | 
         for Name ¡®=¡¯ exp ¡®,¡¯ exp [¡®,¡¯ exp] do block end | 
         for namelist in explist do block end | 
         function funcname funcbody | 
         local function Name funcbody | 
         local namelist [¡®=¡¯ explist] 

    laststat ::= return [explist] | break

    funcname ::= Name {¡®.¡¯ Name} [¡®:¡¯ Name]

    varlist ::= var {¡®,¡¯ var}

    var ::=  Name | prefixexp ¡®[¡¯ exp ¡®]¡¯ | prefixexp ¡®.¡¯ Name 

    namelist ::= Name {¡®,¡¯ Name}

    explist ::= {exp ¡®,¡¯} exp

    exp ::=  nil | false | true | Number | String | ¡®...¡¯ | function | 
         prefixexp | tableconstructor | exp binop exp | unop exp 

    prefixexp ::= var | functioncall | ¡®(¡¯ exp ¡®)¡¯

    functioncall ::=  prefixexp args | prefixexp ¡®:¡¯ Name args 

    args ::=  ¡®(¡¯ [explist] ¡®)¡¯ | tableconstructor | String 

    function ::= function funcbody

    funcbody ::= ¡®(¡¯ [parlist] ¡®)¡¯ block end

    parlist ::= namelist [¡®,¡¯ ¡®...¡¯] | ¡®...¡¯

    tableconstructor ::= ¡®{¡¯ [fieldlist] ¡®}¡¯

    fieldlist ::= field {fieldsep field} [fieldsep]

    field ::= ¡®[¡¯ exp ¡®]¡¯ ¡®=¡¯ exp | Name ¡®=¡¯ exp | exp

    fieldsep ::= ¡®,¡¯ | ¡®;¡¯

    binop ::= ¡®+¡¯ | ¡®-¡¯ | ¡®*¡¯ | ¡®/¡¯ | ¡®^¡¯ | ¡®%¡¯ | ¡®..¡¯ | 
         ¡®<¡¯ | ¡®<=¡¯ | ¡®>¡¯ | ¡®>=¡¯ | ¡®==¡¯ | ¡®~=¡¯ | 
         and | or

    unop ::= ¡®-¡¯ | not | ¡®#¡¯

#endif 

namespace lua
{

    ///
    /// binary operator symbol table
    ///
    template<typename CharT>
    struct binop_symbols :
            boost::spirit::classic::symbols<std::string, CharT>
    {
        binop_symbols()
        {
            boost::spirit::classic::symbols<std::string, CharT> ::
            add
            ( "+"   , "+" )
            ( "-"  , "-" )
            ( "*"   , "*" )
            ( "/"    , "/" )
            ( "^"   , "^" )
            ( ".."  , ".." )
            ( "<" , "<" )
            ( "<="   , "<=" )
            ( ">"   , ">" )
            ( ">="   , ">=" )
            ( "=="   , "==" )
            ( "~="   , "~=" )
            ( "and"   , "and" )
            ( "or"   , "or" )
            ;
        }

    };
    const binop_symbols<char> binop_symbols_p = binop_symbols<char>();

    ///
    /// unary operator symbol table
    ///
    template<typename CharT>
    struct unop_symbols : boost::spirit::classic::symbols<std::string, CharT>
    {
        unop_symbols()
        {
            boost::spirit::classic::symbols<std::string, CharT> ::
            add
            ( "-"   , "-" )
            ( "not"  , "not" )
            ;
        }

    };
    const unop_symbols<char> unop_symbols_p = unop_symbols<char>();

    ///
    /// boolean symbols
    ///
    template<typename CharT>
    struct boolean_symbols : boost::spirit::classic::symbols<std::string, CharT>
    {
        boolean_symbols()
        {
            boost::spirit::classic::symbols<std::string, CharT> ::
            add
            ( "nil"   , "nil" )
            ( "false"  , "false" )
            ( "true"  , "true" )
            ;
        }

    };
    const boolean_symbols<char> boolean_symbols_p = boolean_symbols<char>();

    ///
    /// literal grammar
    ///
    struct literal_grammar : boost::spirit::classic::grammar< literal_grammar >
    {
        template <typename ScannerT>
        struct definition
        {
            boost::spirit::classic::rule<ScannerT>
            escaped_literal,
            literal_double_bracket,
            literal_apos,
            literal_quot,
            literal;

            definition( literal_grammar const& self_ )
            {
                using namespace boost::spirit::classic;
                escaped_literal =
                    ch_p( '\\' ) >> chset_p( "abfnrtv\\\"\'[]" )
                    | anychar_p
                    ;
                literal_double_bracket = comment_p( "[[", "]]" );
                literal_apos = confix_p( '\'', *escaped_literal, '\'' );
                literal_quot = confix_p( '\"', *escaped_literal, '\"' );
                literal =
                    literal_double_bracket
                    | literal_apos
                    | literal_quot
                    ;
                BOOST_SPIRIT_DEBUG_NODE( escaped_literal );
                BOOST_SPIRIT_DEBUG_NODE( literal_double_bracket );
                BOOST_SPIRIT_DEBUG_NODE( literal_apos );
                BOOST_SPIRIT_DEBUG_NODE( literal_quot );
                BOOST_SPIRIT_DEBUG_NODE( literal );
            };

            boost::spirit::classic::rule<ScannerT> const& start() const { return literal; }
        };
    };
    const literal_grammar literal_grammar_p = literal_grammar();

    ///
    /// name grammar
    ///
    struct name_grammar : boost::spirit::classic::grammar< name_grammar >
    {
        template <typename ScannerT>
        struct definition
        {
            boost::spirit::classic::rule<ScannerT>
            name;

            definition( name_grammar const& self_ )
            {
                using namespace boost::spirit::classic;
                name = lexeme_d[ alpha_p >> *( alnum_p | ch_p( '_' ) ) ];
                BOOST_SPIRIT_DEBUG_NODE( name );
            };

            boost::spirit::classic::rule<ScannerT> const& start() const { return name; }
        };
    };
    const name_grammar name_grammar_p = name_grammar();

    ///
    /// the lua grammar
    ///
    class lua_grammar :
        public boost::spirit::classic::grammar< lua_grammar >
    {
        public:

            template <typename ScannerT>
            struct definition
            {
                definition( lua_grammar const& self_ )
                {
                    using namespace boost::spirit::classic;
                    Name = name_grammar_p;
                    Literal = literal_grammar_p;
                    comment = comment_p( "--" );
                    fieldsep = chset_p( ",;" );
                    field =
                        (
                            (
                                confix_p( ch_p( '[' ), exp, ch_p( ']' ) )
                                >> ch_p( '=' )
                                >> exp
                            )
                            | ( Name >> ch_p( '=' ) >> exp )
                            | exp
                        );
                    fieldlist =
                        (
                            list_p( field, fieldsep ) >> !fieldsep
                        );
                    tableconstructor =
                        (
                            ( ch_p( '{' ) >> ch_p( '}' ) )
                            | confix_p( ch_p( '{' ), fieldlist, ch_p( '}' ) )
                        );
                    funcname =
                        list_p( Name, ch_p( '.' ) )
                        >> !(
                            ch_p( ':' )
                            >> Name
                        );
                    parlist1 =
                        list_p( Name, ch_p( ',' ) )
                        >> !(
                            ( ch_p( ',' ) >> str_p( "..." ) )
                            | str_p( "..." )
                        );
                    funcbody =
                        ( confix_p( ch_p( '(' ),  !parlist1, ch_p( ')' ) )
                          >> block
                          >> str_p( "end" )
                        );
                    function =
                        (
                            str_p( "function" ) >> funcbody
                        );
                    args =
                        (
                            ch_p( '(' ) >> ch_p( ')' )
                            | confix_p( ch_p( '(' ) , explist1, ch_p( ')' ) )
                            | tableconstructor
                            | Literal
                        );
                    functioncallexp =
                        args
                        | ( ch_p( ':' ) >> Name >> args );
                    functioncall =
                        (
                            prefixexp_func >> functioncallexp
                        );
                    varexp =
                        confix_p( ch_p( '[' ), exp, ch_p( ']' ) )
                        | ch_p( '.' ) >> Name;
                    var =
                        (
                            Name
                            | ( prefixexp_var >> varexp )
                        );
                    prefixexp_func =
                        Name
                        >>
                        *( !functioncallexp >>
                           (
                               varexp
                               | confix_p( ch_p( '(' ), exp, ch_p( ')' ) )
                           )
                         )
                        ;
                    prefixexp_var =
                        Name
                        >>
                        *( ! varexp >>
                           (
                               functioncallexp
                               | confix_p( ch_p( '(' ), exp, ch_p( ')' ) )
                           )
                         )
                        ;
                    prefixexp =
                        Name
                        >>
                        *( varexp
                           | functioncallexp
                           | confix_p( ch_p( '(' ), exp, ch_p( ')' ) )
                         )
                        ;
                    varlist1 =
                        (
                            list_p( var, ch_p( ',' ) )
                        );
                    do_loop =
                        (
                            str_p( "do" ) >> block >> str_p( "end" )
                        );
                    while_loop =
                        (
                            str_p( "while" ) >> exp >> str_p( "do" ) >> block >> str_p( "end" )
                        );
                    repeat_loop =
                        (
                            str_p( "repeat" ) >> block  >> str_p( "until" ) >> exp
                        );
                    if_loop =
                        (
                            str_p( "if" ) >> exp >> str_p( "then" ) >> block
                            >> *( str_p( "elseif" ) >> exp >> str_p( "then" ) >> block )
                            >> !( str_p( "else" ) >> block )
                            >> str_p( "end" )
                        );
                    return_ =
                        (
                            str_p( "return" ) >> !explist1
                        );
                    for_loop =
                        (
                            str_p( "for" ) >> Name >> ch_p( '=' ) >> exp
                            >> ch_p( ',' ) >> exp
                            >> !( ch_p( ',' ) >> exp )
                            >> str_p( "do" ) >> block  >> str_p( "end" )
                        );
                    for_loop_in =
                        (
                            str_p( "for" ) >> list_p( Name, ch_p( ',' ) )
                            >> str_p( "in" ) >> explist1
                            >> str_p( "do" ) >> block >> str_p( "end" )
                        );
                    local_function =
                        (
                            str_p( "local" ) >> str_p( "function" ) >> Name >> funcbody
                        );
                    local_namelist =
                        (
                            str_p( "local" ) >> namelist >> !init
                        );
                    function_full =
                        (
                            str_p( "function" ) >> funcname >> funcbody
                        );
                    namelist =
                        (
                            list_p( Name, ch_p( ',' ) )
                        );
                    explist1 =
                        (
                            exp
                            >> !(
                                ch_p( ',' )
                                >> list_p( exp, ',' )
                            )
                        );
                    init = ch_p( '=' ) >> explist1;
                    stat =
                        (
                            ch_p( ';' )
                            | ( varlist1 >> ch_p( '=' ) >> explist1 )
                            | functioncall
                            | do_loop
                            | while_loop
                            | repeat_loop
                            | if_loop
                            | return_
                            | str_p( "break" )
                            | for_loop
                            | for_loop_in
                            | function_full
                            | local_function
                            | local_namelist
                            | comment
                        );
                    ;
                    chunk =
                        (
                            *( stat >> !ch_p( ';' ) )
                        );
                    block =
                        (
                            chunk
                        );
                    exp_inner =
                        boolean
                        | real_p
                        | Literal
                        | function
                        | tableconstructor
                        | ( unop >> exp )
                        | prefixexp;
                    exp =
                        (
                            exp_inner >> *( binop >> exp )
                            | unop >> exp
                        );
                    BOOST_SPIRIT_DEBUG_NODE( binop );
                    BOOST_SPIRIT_DEBUG_NODE( unop );
                    BOOST_SPIRIT_DEBUG_NODE( boolean );
                    BOOST_SPIRIT_DEBUG_NODE( chunk );
                    BOOST_SPIRIT_DEBUG_NODE( block );
                    BOOST_SPIRIT_DEBUG_NODE( stat );
                    BOOST_SPIRIT_DEBUG_NODE( funcname );
                    BOOST_SPIRIT_DEBUG_NODE( varlist1 );
                    BOOST_SPIRIT_DEBUG_NODE( var );
                    BOOST_SPIRIT_DEBUG_NODE( varexp );
                    BOOST_SPIRIT_DEBUG_NODE( namelist );
                    BOOST_SPIRIT_DEBUG_NODE( init );
                    BOOST_SPIRIT_DEBUG_NODE( explist1 );
                    BOOST_SPIRIT_DEBUG_NODE( exp );
                    BOOST_SPIRIT_DEBUG_NODE( exp_inner );
                    BOOST_SPIRIT_DEBUG_NODE( prefixexp );
                    BOOST_SPIRIT_DEBUG_NODE( prefixexp_func );
                    BOOST_SPIRIT_DEBUG_NODE( prefixexp_var );
                    BOOST_SPIRIT_DEBUG_NODE( functioncall );
                    BOOST_SPIRIT_DEBUG_NODE( functioncallexp );
                    BOOST_SPIRIT_DEBUG_NODE( args );
                    BOOST_SPIRIT_DEBUG_NODE( function );
                    BOOST_SPIRIT_DEBUG_NODE( funcbody );
                    BOOST_SPIRIT_DEBUG_NODE( parlist1 );
                    BOOST_SPIRIT_DEBUG_NODE( tableconstructor );
                    BOOST_SPIRIT_DEBUG_NODE( fieldlist );
                    BOOST_SPIRIT_DEBUG_NODE( field );
                    BOOST_SPIRIT_DEBUG_NODE( fieldsep );
                    BOOST_SPIRIT_DEBUG_NODE( do_loop );
                    BOOST_SPIRIT_DEBUG_NODE( while_loop );
                    BOOST_SPIRIT_DEBUG_NODE( repeat_loop );
                    BOOST_SPIRIT_DEBUG_NODE( if_loop );
                    BOOST_SPIRIT_DEBUG_NODE( return_ );
                    BOOST_SPIRIT_DEBUG_NODE( for_loop );
                    BOOST_SPIRIT_DEBUG_NODE( for_loop_in );
                    BOOST_SPIRIT_DEBUG_NODE( local_function );
                    BOOST_SPIRIT_DEBUG_NODE( local_namelist );
                    BOOST_SPIRIT_DEBUG_NODE( function_full );
                    BOOST_SPIRIT_DEBUG_NODE( comment );
                    BOOST_SPIRIT_DEBUG_NODE( Name );
                    BOOST_SPIRIT_DEBUG_NODE( real_p );
                    BOOST_SPIRIT_DEBUG_NODE( Literal );
                };
                boost::spirit::classic::rule<ScannerT> const& start() const { return chunk; }

                binop_symbols<char> binop;
                unop_symbols<char> unop;
                boolean_symbols<char> boolean;

                boost::spirit::classic::rule<ScannerT>
                Name,
                Literal,
                chunk,
                block,
                stat,
                funcname,
                varlist1,
                prefvar,
                var,
                varexp,
                namelist,
                init,
                explist1,
                exp_inner,
                exp,
                prefixexp,
                prefixexp_func,
                prefixexp_var,
                functioncall,
                functioncallexp,
                args,
                function,
                funcbody,
                parlist1,
                tableconstructor,
                fieldlist,
                field,
                fieldsep,
                do_loop,
                while_loop,
                repeat_loop,
                if_loop,
                return_,
                for_loop,
                for_loop_in,
                local_function,
                local_namelist,
                function_full,
                comment,
                //!!new!!
                varlist,
                explist,
                laststat
                ;
            };

    };

    ///
    /// a small test function...
    /// returns a "home made" parse_info to avoid including spirit headers
    ///
    my_parse_info test_lua_grammar( std::string const& file_name_ )
    {
        using namespace boost::spirit::classic;
        file_iterator<> first( file_name_.c_str() );

        if ( !first )
            { throw std::invalid_argument( std::string( "could not open file" ) ); }

        file_iterator<> last = first.make_end();
        typedef char                    char_t;
        typedef file_iterator <char_t>  iterator_t;
        lua_grammar lua_rule;
        parse_info<iterator_t> info = parse( first, last, lua_rule, space_p );
        my_parse_info pi;
        pi.hit = info.hit;
        pi.full = info.full;
        pi.length = info.length;
        return pi;
    };

};

#endif//_LUA_GRAMMAR_HPP_INCLUDED_SOD8FU3498USFDLKJSFDLKJ4OUISDFLKJSLFKJASODIUJ4OIUJSFDKLJOIJSFDLKJSLKJDOFIJ4OIEWOIDKDKDF
