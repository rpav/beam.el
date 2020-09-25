# beam.el

This is a very small replacement for Projectile.  It does the few things I want and tries to be as dumb as possible:

* Project switching and "landing page" (a dired buffer)
* Project file finding
* Simple `projectile-project-root` and `projectile-project-name` replacement functions.
* Supports Ivy, Helm, and Ido.

It is not intended to be compatible with projectile; it redefines the above functions.  Remove that and install this.  I have not run into compatibility issues.


## Projects

Projects in Beam are very simple: they have a `.project-root` file at the top level directory of the project.  That's all.

To add a project to Beam's "known" list of projects, use `M-x beam-add-project`; if it doesn't find a `.project-root`, it will offer to create one for you.  There is also `M-x beam-remove-project`.

* `M-x beam-switch-project`: Offer a list of known projects to switch to.  This will by default show `dired` for the project's root.  If you want projectile-like behavior to query a file instead, you can customize `beam-project-landing-function` to the following...
* `M-x beam-find-in-project`: Present a list of files to find in the project.  By default this will also add it to your find-file history.

If you make a `.beamignore` file in the project root, this will be exported as `BEAM_IGNORE_FILE` to the configured command; this can be useful for ignoring a different set of files than the ones you commit.


## Compatibility

This provides two functions: `projectile-project-root` and `projectile-project-name`, that in turn call `beam-project-root` and `beam-project-name`.  It also `(provide 'projectile)`.  This seems to be sufficient compatibility for every projectile-requiring package I have encountered.


## Why?

Projectile has a lot of "smart" behavior and features I find tend to break regularly.  This does everything I want in under 10k (as of writing...).
