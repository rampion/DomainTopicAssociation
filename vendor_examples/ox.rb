#!/usr/bin/env ruby
require 'rubygems'
require 'stringio'
require 'ox'

class Sample < ::Ox::Sax
  def initialize
    @indentation = 0
  end
  def show msg
    print("  " * @indentation)
    puts msg
  end
  private :show
  def start_element(name)
    show "start: #{name}"
    @indentation += 1
	end
  def end_element(name)
    @indentation -= 1
    show "end: #{name}"
	end
  def attr(name, value)
    show "- #{name} => #{value}"
	end
  def text(value)
    show "text: #{value.inspect}"
	end
end

io = StringIO.new(%{
<top name="sample" value="keylime">
  <middle name="second">
    <bottom name="third">&#8;</bottom>
  </middle>
</top>
})

handler = Sample.new()
Ox.sax_parse(handler, io)
# outputs
# start: top
#   name => sample
# start: middle
#   name => second
# start: bottom
#   name => third
# end: bottom
# end: middle
# end: top
