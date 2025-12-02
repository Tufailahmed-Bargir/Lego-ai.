"use client"

import Link from "next/link"
import { usePathname } from "next/navigation"

export function Navbar() {
  const pathname = usePathname()

  const isActive = (path: string) => pathname === path

  return (
    <nav
      className="fixed top-0 left-0 right-0 bg-white shadow-md px-4 py-2 flex justify-between items-center z-50"
      role="navigation"
      aria-label="Main navigation"
    >
      <Link href="/landing" className="flex items-center gap-1">
        <h1 className="text-xl font-bold text-gray-900">Lego</h1>
        <span className="bg-[#4F46E5] text-white text-xs font-bold px-1.5 py-0.5 rounded">AI</span>
      </Link>

      <div className="flex items-center gap-6">
        <div className="hidden md:flex items-center gap-6">
          <Link
            href="/landing#features"
            className={`text-gray-600 hover:text-[#A855F7] transition-colors ${
              isActive("/landing") ? "text-[#A855F7] border-b-2 border-[#A855F7]" : ""
            }`}
          >
            Features
          </Link>
          <Link href="/landing#how-it-works" className="text-gray-600 hover:text-[#A855F7] transition-colors">
            How it works
          </Link>
          <Link href="/landing#pricing" className="text-gray-600 hover:text-[#A855F7] transition-colors">
            Pricing
          </Link>
        </div>

        <div className="flex items-center gap-3">
          <Link
            href="/history"
            className="bg-[#4F46E5] text-white px-4 py-2 rounded-md hover:bg-[#4338CA] transition-colors font-medium"
          >
            History
          </Link>
          <button
            className="bg-[#4F46E5] text-white px-4 py-2 rounded-md hover:bg-[#4338CA] transition-colors font-medium"
            aria-label="Logout"
          >
            Logout
          </button>
        </div>
      </div>
    </nav>
  )
}
