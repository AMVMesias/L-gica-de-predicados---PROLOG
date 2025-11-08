# Diagnóstico de Otitis

Repositorio local para el proyecto "Diagnóstico de Otitis".

Contenido:
- `agente_otitis.py` — Lógica del agente (BFS/DFS, grafo de síntomas).
- `app.py` — Interfaz gráfica con tkinter y visualización con matplotlib/networkx.

Cómo convertir esto en un repositorio de GitHub (resumen):

1. Inicializar git localmente (ya incluido en este repo local):
   ```powershell
   git init
   git add .
   git -c user.email="you@example.com" -c user.name="Your Name" commit -m "Initial commit"
   git branch -M main
   ```

2. Crear el repositorio remoto en GitHub (dos opciones):
   - Usando la web: crear nuevo repo en github.com y copiar la URL remota.
   - Usando `gh` (GitHub CLI):
     ```powershell
     gh repo create NOMBRE-REPO --public --source=. --remote=origin --push
     ```

3. Si usaste la web, enlaza y empuja:
   ```powershell
   git remote add origin https://github.com/TU_USUARIO/NOMBRE-REPO.git
   git push -u origin main
   ```

Dependencias (ver `requirements.txt`).

Notas:
- `tkinter` suele venir con Python en Windows (no se instala vía pip).
- Asegúrate de configurar tu nombre y correo de git si quieres que los commits usen tu identidad real.
