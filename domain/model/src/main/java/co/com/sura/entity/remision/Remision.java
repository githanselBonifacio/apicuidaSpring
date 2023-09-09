package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

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
