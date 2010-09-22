package net_alchim31_vscaladoc2_www.model

import java.net.URL
import java.util.Date

case class Made(by: String, at: Date)
case class RemoteApiInfo(artifactId: String, version: String, baseUrl: URL, provider: ApiProvider, made: Made)
