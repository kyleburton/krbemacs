namespace :js2 do
  desc "Pull update for js2-mode"
  task :update do
    github_project = "https://github.com/mooz/js2-mode"
    url = "https://raw.github.com/mooz/js2-mode/master/js2-mode.el"
    system "wget", url
    FileUtils.mv "js2-mode.el", "lib"
  end
end
