package co.com.sura.entity.remision;

import co.com.sura.entity.maestro.Ciudad;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Remision {
    private String idRemision;
    private String estado;
    private String numeroIdentificacionPaciente;
    private String paciente;
    private LocalDate fechaAdmision;
    private String programa;
    private String ciudad;
    private String tipoAdmision;
    private String institucionRemite;
}
