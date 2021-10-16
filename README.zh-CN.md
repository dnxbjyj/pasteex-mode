[English](./README.md) | 简体中文

# 这是什么？
这是一款GNU Emacs插件，使用这款插件，你可以一键把剪贴板中的图片保存为硬盘文件，并且同时把该文件的链接（对于org-mode或markdown-mode而言），或文件路径（对于其他的mode而言）快速插入到当前的位置。

支持在Windows系统或Mac系统使用，基于PasteEx（Windows系统）和pngpaste（Mac系统）。

下面是一个用法示例：

![](./img/illustrate.gif)

# 提前准备
- Windows系统: 安装[PasteEx](https://github.com/huiyadanli/PasteEx/releases)
- Mac系统: 安装[Pngpaste](https://github.com/jcsalterego/pngpaste)

# 安装插件
下载`pasteex-mode.el`文件，放在`load-path`加载路径，`load-path`加载路径一般是`~/elisp/`路径。在`~/.emacs`文件中做如下配置：

```emacs-lisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'pasteex-mode)
```

# 基本用法
- Windows系统：把`PasteEx.exe`可执行文件的路径加入环境变量，或者在配置文件中设置变量`pasteex-executable-path`的值，像这样设置：

```emacs-lisp
(setq pasteex-executable-path "/path/to/PasteEx.exe")
```

- Mac系统：使用Home Brew安装pngpaste，然后把pngpaste可执行文件加入环境变量，或者在配置文件中设置变量`pasteex-macos-executable-path`的值，像这样设置：

```emacs-lisp
(setq pasteex-macos-executable-path "/path/to/pngpaste")
```

- 绑定你喜欢的快捷键到`pasteex-image`函数，像这样：

```emacs-lisp
(global-set-key (kbd "C-x p i") 'pasteex-image)
```

- 这时，当你截屏到剪贴板，或复制了一个PNG图片文件到剪贴板，然后按下快捷键`C-x p i`，剪贴板中的图片就会被保存到硬盘文件中，并且文件的链接或路径就会被自动插入到当前buffer里，文件默认会被存储到`./img/`路径下。

# 功能列表
当前支持下面的函数：
- `pasteex-image`：保存剪贴板图片到硬盘文件，并把文件的路径插入到当前位置。
- `pasteex-delete-img-link-and-file-at-line`：删除当前行的图片链接，并且同时把关联的硬盘文件也删掉。
- `pasteex-is-png-file`：检查一个文件是否是PNG文件。

