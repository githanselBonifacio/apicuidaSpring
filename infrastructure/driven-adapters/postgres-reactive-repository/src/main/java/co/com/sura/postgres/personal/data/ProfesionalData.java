package co.com.sura.postgres.personal.data;

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
@Table(name = "profesionales")
public class ProfesionalData {

    @Column("numero_identificacion")
    @Id
    private String numeroIdentificacion;

    @Column("id_tipo_identificacion")
    private Integer idTipoIdentificacion;

    private String nombres;
    private String apellidos;
    private String email;
    private String telefono;
    private String celular;
    private String direccion;
    private String genero;

    @Column(value = "id_profesion")
    private Integer idProfesion;

    @Column("fecha_nacimiento")
    private LocalDate fechaNacimiento;

    @Column("id_regional")
    private String idRegional;
    private boolean activo;
}
