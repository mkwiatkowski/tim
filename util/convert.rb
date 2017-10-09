# Converter of timert format to tim format.
#
# Usage:
#   ruby convert.rb TIMERT_JSON_FILE_PATH > output.json

require 'json'

def add_default_project(record)
  record.merge({'project' => 'default'})
end

puts JSON.load(File.open(ARGV.first, 'r')).
  flat_map {|_, day| day['intervals'].map{|r| add_default_project(r)}}.
  to_json
