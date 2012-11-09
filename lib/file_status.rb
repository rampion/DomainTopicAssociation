require 'rubygems'
require 'ruby-progressbar'

module FileStatus
  STATUS_FORMAT =  "%t: [%B] (%a, %E)"

  # simple status display for processing input files
  #
  # Users can use CTRL-C to interrupt input processing
  # (but continue the script).
  #
  #   prefix - prefix to display on each line
  #   options
  #     file    - file object to use for progress checks
  #     total   - number of progress checks to make
  #
  # yields
  #
  #   progress - proc to call at each progress check point
  #
  # returns boolean 
  #   true iff file processing was interrupted
  def self.status(prefix, options)
    interrupted = false
    prev = Signal.trap("INT") do
      exit if interrupted # double CTRL-C to quit full program
      interrupted = true
    end

    if options[:file]
      # if using a file check the location relative to the total
      file      = options[:file]
      total     ||= file.stat.size
      callback  ||= lambda do
        prog.progress = file.tell
        throw :interrupted if interrupted
      end
    else
      # if given an absolute total, just increment
      total     ||= options[:total]
      callback  ||= lambda do
        prog.progress.increment
        throw :interrupted if interrupted
      end
    end

    prog = ProgressBar.create( title: prefix, format: STATUS_FORMAT, total: total )
    catch(:interrupted) { yield callback }
    prog.stop

    # restore previous handler
    Signal.trap("INT", prev)

    # return value
    interrupted
  end
end
