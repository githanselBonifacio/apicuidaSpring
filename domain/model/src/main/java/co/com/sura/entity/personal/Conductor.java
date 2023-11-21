package co.com.sura.entity.personal;

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
public class Conductor {
    private Integer idTipoIdentificacion;
    private String  numeroIdentificacion;
    private String nombres;
    private String apellidos;
    private String email;
    private String telefono;
    private String celular;
    private String direccion;
    private String genero;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
    private LocalDate fechaNacimiento;
    private String idRegional;
    private boolean activo;
}
