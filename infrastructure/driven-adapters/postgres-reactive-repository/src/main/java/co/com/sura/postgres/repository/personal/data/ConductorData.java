package co.com.sura.postgres.repository.personal.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "conductores")
public class ConductorData {

    @Column("id_tipo_identificacion")
    private Integer idTipoIdentificacion;
    @Id
    @Column("numero_identificacion")
    private String  numeroIdentificacion;
    private String nombres;
    private String apellidos;
    private String email;
    private String telefono;
    private String celular;
    private String direccion;
    private String genero;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
    @Column("fecha_nacimiento")
    private LocalDate fechaNacimiento;

    @Column("id_regional")
    private String idRegional;
    private boolean activo;
}
