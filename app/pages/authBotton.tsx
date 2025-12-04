"use client";

import Link from "next/link";
import { useSession, signOut } from "next-auth/react";

export default function AuthButton() {
  const { data: session } = useSession();

  const handleSubmit = async () => {
    await signOut({ callbackUrl: "/" });
  };

  return (
    <div className="hidden md:flex">
      {!session ? (
        <Link href="/login">
          <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition">
            Login
          </button>
        </Link>
      ) : (
        <>
          <Link href={"/history"}>
            <button
              type="button"
              className="px-4 mx-2 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition"
            >
              History
            </button>
          </Link>
          <button
            onClick={handleSubmit}
            type="button"
            className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition"
          >
            Logout
          </button>
        </>
      )}
    </div>
  );
}
