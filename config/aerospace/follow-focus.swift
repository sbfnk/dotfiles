// Switch to an app's AeroSpace workspace when it is activated but AeroSpace
// stays put.
//
// AeroSpace follows focus to another workspace only when the focused window
// is one it tracks. Dialogs it does not track (Open/Save sheets, update and
// auth prompts) can become the key window instead, and the app then holds
// focus while all its windows sit hidden on another workspace. This watches
// app activations and, when that happens, switches to the workspace holding
// the app's windows.

import AppKit

let aerospace = ["/opt/homebrew/bin/aerospace", "/usr/local/bin/aerospace"]
  .first { FileManager.default.isExecutableFile(atPath: $0) } ?? "aerospace"

// Long enough for AeroSpace to finish its own workspace switch, so this only
// acts once AeroSpace has had its chance.
let settleDelay = 0.4

func run(_ args: [String]) -> [String] {
  let process = Process()
  let pipe = Pipe()
  process.executableURL = URL(fileURLWithPath: aerospace)
  process.arguments = args
  process.standardOutput = pipe
  process.standardError = FileHandle.nullDevice
  do { try process.run() } catch { return [] }
  let data = pipe.fileHandleForReading.readDataToEndOfFile()
  process.waitUntilExit()
  guard process.terminationStatus == 0 else { return [] }
  return String(decoding: data, as: UTF8.self)
    .split(separator: "\n").map(String.init)
}

// Clicking the desktop activates Finder, which would otherwise pull you to
// whichever workspace holds a Finder window.
let ignoredBundleIDs: Set = ["com.apple.finder"]

func isFrontmost(_ app: NSRunningApplication) -> Bool {
  NSWorkspace.shared.frontmostApplication?.processIdentifier
    == app.processIdentifier
}

// The app's frontmost window, from the window server's front-to-back list.
// Reading IDs, owners and layers needs no permission, unlike asking the
// accessibility API for the key window. App-modal panels (alerts, Open/Save
// panels run modally) sit at the modal-panel level rather than with normal
// windows, so both levels count.
let windowLayers = [0, Int(CGWindowLevelForKey(.modalPanelWindow))]

func frontWindowID(of pid: pid_t) -> Int? {
  guard let info = CGWindowListCopyWindowInfo(
    [.optionOnScreenOnly, .excludeDesktopElements], kCGNullWindowID)
    as? [[String: Any]]
  else { return nil }
  return info.first {
    ($0[kCGWindowOwnerPID as String] as? pid_t) == pid
      && windowLayers.contains($0[kCGWindowLayer as String] as? Int ?? -1)
  }?[kCGWindowNumber as String] as? Int
}

func followIfStranded(_ app: NSRunningApplication) {
  guard isFrontmost(app),
    !ignoredBundleIDs.contains(app.bundleIdentifier ?? ""),
    let front = frontWindowID(of: app.processIdentifier)
  else { return }

  var appWorkspaces = Set<String>()
  var tracked = Set<Int>()
  for line in run([
    "list-windows", "--monitor", "all", "--pid", String(app.processIdentifier),
    "--format", "%{window-id} %{workspace}",
  ]) {
    let fields = line.split(separator: " ", maxSplits: 1).map(String.init)
    guard fields.count == 2, let id = Int(fields[0]) else { continue }
    tracked.insert(id)
    appWorkspaces.insert(fields[1])
  }

  // When the app's front window is one AeroSpace tracks, AeroSpace does its
  // own following; switching here as well fights it and bounces focus back
  // to the previous app. Act only when an untracked dialog is in front.
  guard !tracked.contains(front) else { return }

  let visible = Set(run(["list-workspaces", "--monitor", "all", "--visible"]))

  // Leave alone apps with no tracked windows (menu bar apps, launchers), apps
  // already showing on some monitor, and apps spread over several hidden
  // workspaces, where there is no single right answer.
  guard appWorkspaces.count == 1, !visible.isEmpty,
    appWorkspaces.isDisjoint(with: visible),
    let target = appWorkspaces.first,
    // A cmd-tab during the checks above has moved on; do not pull it back.
    isFrontmost(app)
  else { return }

  FileHandle.standardError.write(
    "\(Date()) \(app.localizedName ?? "?") -> workspace \(target)\n"
      .data(using: .utf8)!)
  _ = run(["workspace", target])
}

NSWorkspace.shared.notificationCenter.addObserver(
  forName: NSWorkspace.didActivateApplicationNotification,
  object: nil, queue: .main
) { note in
  guard let app = note.userInfo?[NSWorkspace.applicationUserInfoKey]
    as? NSRunningApplication else { return }
  DispatchQueue.main.asyncAfter(deadline: .now() + settleDelay) {
    followIfStranded(app)
  }
}

RunLoop.main.run()
