# Emacs Server 全局单例 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> 本文件替换此前 review 中“多 daemon 隔离 / 安全与运行时边界”的推荐实施顺序。

**Goal:** 同一主机、同一 OS 用户只有一个正式 Emacs Server；Agent Editor 作为该 Server 的常驻单一 Runtime；Stable 与 Snapshot CI 全部绿色。

**Architecture:** 一个长期运行的 `emacs --daemon=main`，承载全局 Buffer / Project / Session、单一 Agent Editor MCP Runtime（固定端口 9876）、多动态 Project、任意绝对路径文件，以及多个 GUI/TTY `emacsclient` frame。CI / batch / 临时隔离进程不受单例限制。

**Tech Stack:** GNU Emacs 30.2+ / Snapshot、Elpaca、Agent Editor MCP、launchd / systemd user services、XDG state/cache、ERT CI。

---

## 明确不在范围内

- 不修改当前 Agent Editor 的认证和授权策略
- 不引入 token authentication 默认值调整
- 不改用 ephemeral port
- 不设计多个 Agent Editor daemon
- 不做 per-daemon desktop 隔离
- 不关闭 Agent Editor 自动启动
- 不延迟加载 Agent Editor
- 不保留或迁移 Org Mobile 配置

## 运行模型

```text
每台主机、每个用户只有一个长期运行的 Emacs Server
    ├── 一个全局 Buffer / Project / Session Runtime
    ├── 一个 Agent Editor MCP Runtime
    ├── 一个 MCP Endpoint
    ├── 多个动态注册的 Project
    ├── 任意直接打开的本地文件
    └── 多个 GUI / TTY emacsclient Frame
```

## 实施顺序（总览）

1. 恢复 Emacs Snapshot 全绿
2. 建立全局唯一 Emacs Server
3. 将 Agent Editor 固定为该 Server 的单一常驻 Runtime
4. 将所有 GUI、TTY 和文件打开入口切换到 emacsclient
5. 将 Desktop 改为 Server 全局单 Session
6. 修复非文件 Buffer identity 和 per-frame UI capability
7. 缓存 breadcrumb、VC 和 Flymake redisplay 数据
8. 完成 envrc/Eglot 生命周期闭环
9. 删除 Org Mobile
10. 清理 Org Capture Frame 生命周期
11. 收敛 Language Registry

---

## 第一批：恢复 Emacs Snapshot 全绿

最高优先级；不与其他结构调整混合提交。

### 目标 CI jobs

```text
Emacs 30.2 / Ubuntu
Emacs 30.2 / macOS
Standalone lint
Emacs snapshot
Agent Editor MCP tests
```

### 已知代码热点与 CI 实证（2026-08-06）

最新 CI run `30999144582`（main）结果：

| Job | 结论 |
| --- | --- |
| Emacs 30.2 / Ubuntu | success |
| Emacs 30.2 / macOS | success |
| Standalone lint | success |
| Emacs snapshot | **failure**（advisory，故 workflow 仍 success） |
| Agent Editor MCP tests（嵌在 suite 末尾） | Snapshot 上失败 |

#### Snapshot 实际失败原因

```text
失败的测试名称:
  emacs-agent-semantic-runtime-capabilities-reject-etags-without-tags

失败文件和行号:
  emacs.d/site-lisp/agent-editor-mcp/test/emacs-agent-semantic-test.el:1105
  断言: (should (eq (alist-get 'backend_present xref) t))
  实际: (eq :false t)

稳定版与 Snapshot 的行为差异:
  Emacs 30.2: (defun etags--xref-backend () 'etags)  — 无 tags 也返回 etags
  Emacs 31+/master: (defun etags--xref-backend ()
                      (when (or tags-table-list tags-file-name) 'etags))
                    — 无 tags 时返回 nil，xref-find-backend 也为 nil

分类:
  公开 API 行为变化（etags--xref-backend）+ 测试假设错误
  不是 byte compiler 变化；一方 ERT 与 warnings-as-errors 在 Snapshot 上已通过
```

一方模块原先怀疑的 alias / Flymake 私有 API **不是当前 CI 失败点**（Snapshot byte-compile 已绿），但仍按计划硬化，避免后续 Snapshot 回归。

### Task 1: 复现 Snapshot 失败

**Files:**
- Reference: `.github/workflows/test-emacs.yml`
- Reference: `./run-emacs-tests.sh`, `./lint-emacs-config.sh`

- [x] **Step 1:** 用与 CI 相同的 Snapshot 版本、启动参数、测试环境本地或容器复现
- [x] **Step 2:** 记录并输出：失败测试名、文件与行号、稳定版 vs Snapshot 行为差异、分类（公开 API / byte compiler / 测试假设）
- [x] **Step 3:** 基于证据列出最小修复集；不得仅凭 exit code 屏蔽 warning

### Task 2: 收敛 Language Tools API

**Files:**
- Modify: `emacs.d/lisp/gsmlg-language-tools.el`
- Possibly modify: callers/tests still mocking `gsmlg-eglot-find-executable`
- Test: `emacs.d/tests/language-modes-test.el`, `emacs.d/tests/tramp-test.el`

- [x] **Step 1:** 将 `gsmlg-language-tools-available-command` / `gsmlg-language-tools-command-executable-p` 内部调用改为 `gsmlg-language-tools-find-executable`
- [x] **Step 2:** 保留 `defalias` 仅供外部兼容；新内部实现不得依赖 alias
- [x] **Step 3:** 跑相关 ERT + byte-compile warnings-as-errors

### Task 3: 隔离 Flymake 私有 API

**Files:**
- Create or modify: compatibility adapter（建议 `emacs.d/lisp/gsmlg-compat.el` 或既有 compat 模块）
- Modify: `emacs.d/lisp/gsmlg-ui.el`
- Test: `emacs.d/tests/ui-test.el`

- [x] **Step 1:** 优先改用公开 API；若不足，实现 `gsmlg-compat-flymake-diagnostic-severity`
- [x] **Step 2:** 所有版本判断、`fboundp`、fallback 集中在 adapter；UI 只调稳定接口
- [x] **Step 3:** 确认 UI 不再直接引用 `flymake--*`

### Task 4: 清理其他 Snapshot 风险并恢复门禁

**Files:**
- Modify: `.github/workflows/test-emacs.yml`（Snapshot `advisory`）
- Modify: 其他按复现结果命中的文件

- [x] **Step 1:** 扫描并修复：私有函数、未声明动态变量、编译期 alias、精确错误字符串、内部对象结构、参数列表变化、新 byte-compile warning
- [x] **Step 2:** 禁止：全局关 `byte-compile-error-on-warn`、跳过整个 Snapshot suite、大范围 `no-byte-compile`、无条件 `condition-case` 吞错
- [x] **Step 3:** Snapshot 连续多次绿色后，将仓库兼容性回归改为阻止合并的有效门禁

### 第一批验收

```text
稳定版和 Snapshot 执行相同的一方模块测试
warnings-as-errors 继续开启
没有新增全局 warning suppression
没有直接新增 Emacs 私有 API 依赖
连续多次 CI 执行结果稳定
```

---

## 第二批：Emacs Server 全局单例与统一 Session

### 架构决策

> 同一台主机、同一操作系统用户，只运行一个正式的交互式 Emacs Server 进程。

固定 server name：`main`。

```text
OS User Service
    └── emacs --daemon=main
            ├── Agent Editor MCP :9876
            ├── Desktop / Session
            ├── Project Registry
            ├── Eglot Servers
            ├── Managed Buffers
            └── emacsclient Frames
```

### Task 5: OS 用户服务管理 Server

**Files (expected):**
- Create: launchd user agent（macOS）
- Create: systemd user service（NixOS/Linux）
- Possibly modify: `install.sh` / shell docs

- [x] **Step 1:** 登录后启动唯一 `main` server；异常退出重启；最后 frame 关闭不退出
- [x] **Step 2:** 正常关机保存 session；固定 XDG state/cache/server socket

### Task 6: 入口统一 emacsclient

**Files (expected):**
- Modify: oh-my-zsh aliases / EDITOR / VISUAL / GIT_EDITOR
- Create: 诊断命令（PID、socket、alive、restart、safe quit、GUI/TTY client）

- [x] **Step 1:** GUI/TTY/打开文件/eval 全部 `-s main`
- [x] **Step 2:** 直接独立 GUI Emacs 不再是正式工作流；保留诊断入口

### Task 7: Lisp `gsmlg-server.el`

**Files:**
- Create: `emacs.d/lisp/gsmlg-server.el`
- Modify: `emacs.d/init.el` 加载顺序
- Modify: `emacs.d/lisp/gsmlg-session.el`, `emacs.d/lisp/gsmlg-agent.el`
- Test: server/session focused ERT + `./run-emacs-tests.sh`

- [x] **Step 1:** 定义唯一 server name、启动/验证、identity、health/status、shutdown/session hooks
- [x] **Step 2:** 明确 Server-owned vs frame-owned 状态边界
- [x] **Step 3:** 避免 client frame 创建时重复初始化核心服务

### Task 8: Agent Editor 单次启动与 metadata

**Files:**
- Modify: `emacs.d/lisp/gsmlg-agent.el`
- Modify: `emacs.d/site-lisp/agent-editor-mcp/`（若 connection path / idempotency 需改）
- Test: `emacs.d/tests/agent-test.el` + package tests

- [x] **Step 1:** 仅在正式 Server 进程启动一次；`gsmlg-agent-start` 幂等
- [x] **Step 2:** 固定端口 `9876`；metadata 固定路径 `${XDG_STATE_HOME}/emacs/agent-editor/connection.json`
- [x] **Step 3:** metadata 描述 Editor Server（instance_id/server_name/pid/endpoint/started_at）；project 列表不写入连接文件
- [x] **Step 4:** frame hooks 不得创建 Agent Runtime

### Task 9: 全局单一 Desktop Session

**Files:**
- Modify: `emacs.d/lisp/gsmlg-session.el`
- Modify: `emacs.d/tests/smoke-test.el`（当前断言 `desktop-restore-frames` 为 t）

- [x] **Step 1:** 单一路径 `${XDG_STATE_HOME}/emacs/desktop/desktop.el`
- [x] **Step 2:** `desktop-restore-frames nil`；恢复 buffers/位置/必要 variables；不恢复历史 GUI/TTY frame 与 monitor 坐标
- [x] **Step 3:** client frame 关闭不保存/覆盖 desktop

### Task 10: 测试 profile 绕过单例副作用

**Files:**
- Modify: startup/test harness、agent connection writers
- Env: `GSMLG_EMACS_TESTING=1` 和/或 `noninteractive`

- [x] **Step 1:** 测试不得连接真实 `main`、绑定真实 `9876`、改真实 connection/desktop、因 Server 已存在而退出

### 第二批验收

```text
系统中只有一个正式 Emacs Server PID
所有 GUI 和 TTY frame 报告相同的 emacs-pid
关闭所有 client frame 后 Server 仍然运行
重新打开 client 后原 buffers 仍然存在
Agent Editor 只有一个 listener
Agent Editor 只有一份 connection.json
重复执行 gsmlg-agent-start 不创建第二个 Runtime
多个 project 同时存在于同一个 Project Registry
绝对路径文件可以不依赖 project 直接操作
CI 与 batch test 不连接用户真实 Server
```

本地验证（2026-08-06）：`./run-emacs-tests.sh` 一方 254（252 expected + 2 skipped）/ Agent MCP 229；byte-compile/checkdoc 通过。

---

## 第三批：Session 与 UI 收尾

### Task 11: 非文件 Buffer identity

**Files:**
- Modify: `emacs.d/lisp/gsmlg-ui.el`
- Test: `emacs.d/tests/ui-test.el`

- [ ] **Step 1:** 无 `buffer-file-name` 时显示 `mode-line-buffer-identification`（或等价 segment）
- [ ] **Step 2:** 文件 buffer 不在 header/mode-line 重复路径

### Task 12: Nerd Font 按 frame 保存

**Files:**
- Modify: `emacs.d/lisp/gsmlg-ui.el`

- [ ] **Step 1:** `set-frame-parameter` 保存 `gsmlg-nerd-font-available`
- [ ] **Step 2:** 渲染时检查当前 frame 的 graphic + parameter

### Task 13: 缓存 breadcrumb / VC / Flymake

**Files:**
- Modify: `emacs.d/lisp/gsmlg-ui.el`, possibly `gsmlg-vcs.el`

- [ ] **Step 1:** breadcrumb buffer-local cache，key 含 file/default-directory/project-root；相关事件失效
- [ ] **Step 2:** VC buffer-local cache + 操作后/保存后刷新 + idle timer；TRAMP 更保守
- [ ] **Step 3:** Flymake 一次 `flymake-diagnostics`，单次遍历统计 error/warning/note

### 第三批验收

```text
文件 buffer 不重复显示文件路径
Dired、Help、Compilation 明确显示 buffer 名称
同一 Server 的 GUI 与 TTY frame 使用不同图标能力
TRAMP buffer 的 redisplay 不反复触发 project/VC 重计算
大量 Flymake diagnostics 时 mode-line 不重复扫描
关闭和重新创建 frame 不影响 Server 全局 buffer 状态
```

---

## 第四批：核心功能与生命周期收尾

### Task 14: Agent Editor 明确为核心模块

**Files:**
- Modify: `emacs.d/init.el`, `emacs.d/lisp/gsmlg-agent.el`
- Docs/tests as needed

- [ ] **Step 1:** 常驻加载；Server 启动路径只 `start` 一次
- [ ] **Step 2:** 保留 status/restart/stop/start/show-connection 管理命令
- [ ] **Step 3:** 不做延迟 require / lightweight shim

### Task 15: 删除 Org Mobile

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org.el`, keybindings, README, `emacs.d/tests/org-test.el`

- [ ] **Step 1:** 删除变量、默认 `/Volumes/...` 路径、`org-mobile-*` 设置、命令、keybindings、文档、测试、只为 Mobile 的初始化
- [ ] **Step 2:** 普通 Org 工作流不依赖 Org Mobile

### Task 16: envrc + Eglot 闭环

**Files:**
- Modify: envrc/eglot integration modules（如 `gsmlg-eglot.el` / language tools）

- [ ] **Step 1:** 增加 `gsmlg-envrc-reload-and-refresh-eglot`
- [ ] **Step 2:** reload → 清 negative cache → 未管理则 `eglot-ensure`；已管理则 reconnect/提示

### Task 17: Org Capture Frame 清理

**Files:**
- Modify: `emacs.d/lisp/gsmlg-org.el`
- Test: `emacs.d/tests/org-test.el`

- [ ] **Step 1:** 专用 frame parameter；finalize/abort 都删除
- [ ] **Step 2:** 不用固定 top/left；由 workarea/WM 决定；不影响其他 client frame

### Task 18: Language Registry 职责收敛

**Files:**
- Modify: `emacs.d/lisp/gsmlg-language-registry.el` + consumers/docs

- [ ] **Step 1:** 要么成为 major/ts/lsp/formatter/dap/patterns/markers 统一源，要么缩小文档声明，删除不实的 “single source of truth” 表述

### 第四批验收

```text
Agent Editor 在 Server 启动时已加载
Agent Editor 不依赖首次 MCP 请求或首次 frame 才初始化
启动过程中只创建一个 Agent Runtime
Org Mobile 变量、路径、命令和文档全部删除
envrc reload 后 Eglot 可以重试或 reconnect
Org capture finalize 和 abort 都不会遗留 frame
Language Registry 的文档与真实职责一致
```

---

## Definition of Done

```text
同一用户、同一主机只有一个正式 Emacs Server
所有日常编辑界面都是该 Server 的 emacsclient frame
Agent Editor 是 Server 的常驻核心能力
Agent Editor 只启动一个 Runtime 和一个 MCP listener
Agent 可同时管理多个 project
Agent 可脱离 project 直接操作绝对路径文件
所有 client 共享同一批 buffers、projects、Eglot 和 session
关闭 client frame 不会终止 Server
重新连接后保留编辑状态
Session 不恢复过期 GUI frame
GUI 与 TTY 使用各自的 frame-local 显示能力
Org Mobile 已从配置中移除
Stable 与 Snapshot CI 全部绿色
```
