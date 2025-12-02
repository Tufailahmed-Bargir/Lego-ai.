import type React from "react"
import type { Metadata, Viewport } from "next"
import { Inter } from "next/font/google"
import "./globals.css"

const inter = Inter({ subsets: ["latin"] })

export const metadata: Metadata = {
  title: "Lego AI - Transform Legacy Code into Modern Magic",
  description:
    "AI-powered legacy code converter that transforms outdated COBOL, Delphi, Perl into clean C++, Python, Java – fast, accurate, and scalable.",
  generator: "v0.app",
}

export const viewport: Viewport = {
  themeColor: "#A855F7",
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang="en">
      <body className={`${inter.className} antialiased`}>{children}</body>
    </html>
  )
}
