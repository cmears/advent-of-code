class C
  def initialize(s)
    @l = self.class.lexer(s)
    @p = parseExpr
    @v = eval
    puts(@v)
  end

  def self.lexer(s)
    s.scan(/[0-9]+|[*+()]/)
  end
  def self.ex
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  end

  def parseExpr
    t = parseTerm
    et = parseExprTail
    if et
      ["*", t, et]
    else
      t
    end
  end

  def parseExprTail
    if @l.first == "*"
      @l.shift
      parseExpr
    end
  end

  def parseTerm
    u = parseUnit
    tt = parseTermTail
    if tt
      ["+", u, tt]
    else
      u
    end
  end

  def parseTermTail
    if @l.first == "+"
      @l.shift
      parseTerm
    end
  end

  def parseUnit
    if @l.first == "("
      @l.shift
      e = parseExpr
      n = @l.shift
      raise "!" unless n == ")"
      e
    else
      [@l.shift.to_i]
    end
  end

  def eval
    self.class.eval2(@p)
  end

  def self.eval2(p)
    case p.first
    when "*"
      eval2(p[1]) * eval2(p[2])
    when "+"
      eval2(p[1]) + eval2(p[2])
    else
      p.first
    end
  end
    
end
