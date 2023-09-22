package co.com.sura.entity.agenda;


import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Profesional {
    private Integer idTipoIdentificacion;
    private String numeroIdentificacion;
    private String nombres;
    private String apellidos;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate fechaNacimiento;
    private String idRegional;
    private boolean activo;
}
