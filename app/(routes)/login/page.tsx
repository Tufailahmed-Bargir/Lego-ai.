import LoginPage from "@/app/pages/Login";
import { getServerSession } from "next-auth";
import { redirect } from "next/navigation";

export default async function Login() {
  const session = await getServerSession();
  if (session) {
    redirect("/");
  }
  return (
    <>
      <LoginPage />
    </>
  );
}
