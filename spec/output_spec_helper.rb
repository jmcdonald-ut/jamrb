require 'fileutils'
require 'shellwords'
require 'open3'

module OutputSpecHelper
  module Includes
    def make_executable_in(path)
      navigate_to path
      execute! "make clean"
      execute! "make"
    end

    def execute!(*cmd, &block)
      result, status = Open3.capture2e *cmd

      unless status == 0
        fail IOError, "`$ #{cmd}` exited with non-zero status, see: #{result}"
      end

      [result, status]
    end

    def navigate_to(path)
      FileUtils.cd path
    end
  end

  module Extends
    def fetch_fixtures(path)
      Dir.entries(path)
        .keep_if { |file_name| file? file_name, path }
        .keep_if { |file_name| input_file? file_name }
        .keep_if { |file_name| has_output_file? file_name, path }
        .map { |file_name| File.expand_path(file_name, path) }
    end

    def file?(file_name, path)
      File.file? File.expand_path(file_name, path)
    end

    def input_file?(file_name)
      file_name.match(/.*\.out/).nil?
    end

    def has_output_file?(file_name, path)
      file? "#{file_name}.out", path
    end

    def execute(*cmd, &block)
      Open3.capture2e *cmd
    rescue
      [nil, -1]
    end

    def shellify(string)
      string.shellescape
    end

    def navigate_to(path)
      FileUtils.cd path
    end
  end
end
