require "sinatra"
require 'open3'
require 'json'
set :environment, :production
def gen_tag_list (url)
  stdout = Open3.capture3("/Users/tomoya/git-lab/ma/src/sample.exe", url)
  result =  stdout[0].strip
  lst = result.slice(2, (result.length()-4)).split(", ")

  tag_list = []
  lst.each do |item|
    tag_list << item.slice(8, item.length())
  end
  tag_list.to_json
end

get '/' do
  url = params[:url]
  if url
    gen_tag_list(url)
  else
    "error no params"
  end
end

