package co.com.sura.postgres.repository.agenda.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDateTime;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cita")
public class CitaData {
    @Id
    @Column("id_cita")
    private String idCita;
    @Column("id_remision")
    private String idRemision;
    private Integer duracion;
    private Integer holgura;
    @Column("fecha_inicio")
    private LocalDateTime fechaInicio;
    @Column("fecha_programada")
    private LocalDateTime fechaProgramada;
    private String especialidad;
    @Column("id_ciudad")
    private String idCiudad;
    @Column("id_estado")
    private Integer idEstado;
    @Column("id_profesional")
    private String idProfesional;
    @Column("id_conductor")
    private String idConductor;
}
