"use client"

import Link from "next/link"
import { Navbar } from "@/components/navbar"
import { Sparkles, Languages, Shield } from "lucide-react"

export default function LandingPage() {
  return (
    <div className="min-h-screen">
      <Navbar />

      {/* Hero Section */}
      <section className="min-h-screen bg-gradient-to-br from-purple-50 to-blue-50 flex flex-col justify-center items-center px-4 text-center pt-16">
        <div className="animate-fade-in">
          <h1 className="text-4xl md:text-5xl lg:text-6xl font-bold text-gray-900 mb-6 max-w-4xl text-balance">
            Transform Legacy Code into Modern Magic with Lego AI
          </h1>
          <p className="text-lg md:text-xl text-gray-600 mb-8 max-w-2xl mx-auto text-pretty">
            Our AI-powered converter turns outdated COBOL, Delphi, or Perl into clean C++, Python, Java – fast,
            accurate, and scalable. Say goodbye to maintenance nightmares.
          </p>
          <Link
            href="/convert"
            className="inline-block bg-[#A855F7] text-white px-8 py-4 rounded-lg text-lg font-semibold hover:bg-[#9333EA] transform hover:scale-105 transition-all shadow-lg hover:shadow-xl"
          >
            Start Converting Now
          </Link>
        </div>

        {/* Features Grid */}
        <div
          id="features"
          className="grid grid-cols-1 md:grid-cols-3 gap-8 mt-20 px-4 max-w-6xl mx-auto animate-fade-in-delay"
        >
          <div className="bg-white rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow">
            <div className="w-12 h-12 bg-purple-100 rounded-lg flex items-center justify-center mb-4">
              <Sparkles className="w-6 h-6 text-[#A855F7]" />
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">AI Precision</h3>
            <p className="text-gray-600">95%+ accuracy via advanced models trained on millions of code conversions.</p>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow">
            <div className="w-12 h-12 bg-blue-100 rounded-lg flex items-center justify-center mb-4">
              <Languages className="w-6 h-6 text-[#3B82F6]" />
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">Multi-Language Support</h3>
            <p className="text-gray-600">COBOL to C++, Perl to Python – 10+ language pairs supported.</p>
          </div>

          <div className="bg-white rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow">
            <div className="w-12 h-12 bg-green-100 rounded-lg flex items-center justify-center mb-4">
              <Shield className="w-6 h-6 text-[#10B981]" />
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">Secure & Private</h3>
            <p className="text-gray-600">Your code stays confidential; no data storage. Enterprise-grade security.</p>
          </div>
        </div>
      </section>

      {/* How it Works Section */}
      <section id="how-it-works" className="py-20 px-4 bg-white">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-3xl md:text-4xl font-bold text-center text-gray-900 mb-12">How It Works</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="text-center">
              <div className="w-16 h-16 bg-[#A855F7] text-white rounded-full flex items-center justify-center mx-auto mb-4 text-2xl font-bold">
                1
              </div>
              <h3 className="text-xl font-semibold mb-2">Upload Your Code</h3>
              <p className="text-gray-600">Paste or upload your legacy codebase in any supported format.</p>
            </div>
            <div className="text-center">
              <div className="w-16 h-16 bg-[#A855F7] text-white rounded-full flex items-center justify-center mx-auto mb-4 text-2xl font-bold">
                2
              </div>
              <h3 className="text-xl font-semibold mb-2">Select Target Language</h3>
              <p className="text-gray-600">Choose your desired modern programming language.</p>
            </div>
            <div className="text-center">
              <div className="w-16 h-16 bg-[#A855F7] text-white rounded-full flex items-center justify-center mx-auto mb-4 text-2xl font-bold">
                3
              </div>
              <h3 className="text-xl font-semibold mb-2">Get Clean Code</h3>
              <p className="text-gray-600">Receive optimized, maintainable code with documentation.</p>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing Section */}
      <section id="pricing" className="py-20 px-4 bg-gray-50">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-3xl md:text-4xl font-bold text-center text-gray-900 mb-12">Simple Pricing</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
              <h3 className="text-xl font-semibold mb-2">Starter</h3>
              <p className="text-4xl font-bold text-gray-900 mb-4">
                $29<span className="text-lg text-gray-500">/mo</span>
              </p>
              <ul className="space-y-3 text-gray-600 mb-8">
                <li>✓ 100 conversions/month</li>
                <li>✓ Basic support</li>
                <li>✓ 5 language pairs</li>
              </ul>
              <button className="w-full py-3 border-2 border-[#A855F7] text-[#A855F7] rounded-lg font-semibold hover:bg-[#A855F7] hover:text-white transition-colors">
                Get Started
              </button>
            </div>

            <div className="bg-[#A855F7] rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow transform scale-105">
              <h3 className="text-xl font-semibold mb-2 text-white">Pro</h3>
              <p className="text-4xl font-bold text-white mb-4">
                $99<span className="text-lg text-purple-200">/mo</span>
              </p>
              <ul className="space-y-3 text-purple-100 mb-8">
                <li>✓ Unlimited conversions</li>
                <li>✓ Priority support</li>
                <li>✓ All language pairs</li>
                <li>✓ API access</li>
              </ul>
              <button className="w-full py-3 bg-white text-[#A855F7] rounded-lg font-semibold hover:bg-gray-100 transition-colors">
                Get Started
              </button>
            </div>

            <div className="bg-white rounded-xl shadow-lg p-8 hover:shadow-xl transition-shadow">
              <h3 className="text-xl font-semibold mb-2">Enterprise</h3>
              <p className="text-4xl font-bold text-gray-900 mb-4">Custom</p>
              <ul className="space-y-3 text-gray-600 mb-8">
                <li>✓ Custom integrations</li>
                <li>✓ Dedicated support</li>
                <li>✓ On-premise option</li>
                <li>✓ SLA guarantee</li>
              </ul>
              <button className="w-full py-3 border-2 border-[#A855F7] text-[#A855F7] rounded-lg font-semibold hover:bg-[#A855F7] hover:text-white transition-colors">
                Contact Sales
              </button>
            </div>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="py-12 bg-gray-100">
        <div className="max-w-6xl mx-auto px-4 text-center">
          <p className="text-sm text-gray-500 mb-4">© 2025 Lego AI. Built with love.</p>
          <div className="flex justify-center gap-6">
            <a href="#" className="text-[#A855F7] hover:text-[#9333EA] text-sm transition-colors">
              Privacy Policy
            </a>
            <a href="#" className="text-[#A855F7] hover:text-[#9333EA] text-sm transition-colors">
              Terms of Service
            </a>
          </div>
        </div>
      </footer>
    </div>
  )
}
